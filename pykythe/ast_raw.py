"""Initial processing of lib2to3's AST into an easier form.

The AST that lib2to3 produces is messy to process, so we convert it
into an easier format, defined in ast_cooked. While doing this, we
also mark all bindings (Python requires two passes to resolve local
variables, so this does the first pass).

By default, lib2to3 collapses parent-child nodes where there's a
single child; this is convenient for people writing 2to3 filters but
makes things more complicated for the kind of detailed AST analysis in
this module. Therefore, we define our own _convert function.

Lib2to3 supports both Python2 and Python3 syntax.

The basic usage is:
    parse_tree = ast_raw.parse(src_content, python_version)
    cooked_nodes = ast_raw.cvt_parse_tree(parse_tree, python_version, src_file)

The processing is driven off _DISPATCH[node.type]. Each function is
named cvt_XXX, where XXX is usually derived from the name of the
corresponding grammar rule.
"""

# pylint: disable=too-many-lines
# pylint: disable=too-many-public-methods

import codecs
import collections
from dataclasses import dataclass
import dataclasses
import enum
import io
import logging
import re
import sys
from lib2to3 import pygram
from lib2to3 import pytree
from lib2to3.pygram import python_symbols as syms
from lib2to3.pgen2 import driver, grammar as pgen2_grammar, token, tokenize

from typing import (Callable, Dict, FrozenSet, List, Optional, Sequence, Text, Tuple, Union)  # pylint: disable=unused-import
import typing

# The following requires pip3 install mypy_extensions
# and possibly symlinking into /usr/local/lib/python3.6/dist-packages
from mypy_extensions import Arg

from . import ast, ast_cooked, fakesys, pod, typing_debug
from .typing_debug import cast as xcast


def cvt_parse_tree(
        parse_tree: pytree.Base, python_version: int, src_file: ast.File) -> ast_cooked.Base:
    """Convert a lib2to3.pytree to ast_cooked.Base."""
    return cvt(parse_tree, new_ctx(python_version, src_file))


# pylint: disable=too-few-public-methods
# pylint: disable=no-else-return


class NameCtx(enum.Enum):
    """Context for resolving names.  See Ctx.name_ctx.

    Values:
      BINDING: Appears on the left-hand side of an assignment in a
          position that would result in a binding (e.g., `x = 1` would
          be a binding for `x`, `foo.f = 2` would be a binding for `f`
          but not for `foo`, and `bar[i] = 3` would not be a binding
          for either `bar` or `i`).
      REF: Appears on the right-hand side of an assignment or in a
          position on the left-hand side that is not binding.
      RAW: Appears in an `import` statement in a position where it
          does not get a fully qualified name. For example, in `from
          foo.bar import qqsv as zork`, `foo`, `bar`, `qqsv` are `RAW`
          and `zork` is `BINDING` (and gets a FQN).
    """
    BINDING = 'BINDING'  # TODO: enum.auto()
    REF = 'REF'  # TODO: enum.auto()
    RAW = 'RAW'  # TODO: enum.auto()


@dataclass(frozen=True)
class Ctx(pod.PlainOldData):
    """Context for traversing the lib2to3 AST.

    Note that scope_bindings, global_vars, nonlocal_vars are dicts, so
    they can be updated and therefore Ctx behaves somewhat like a
    mutable object (name_ctx should not be updated; instead a new Ctx
    object should be created using the replace method). For those who
    like functional programming, this is cheating; but Python doesn't
    make it easy to have "accumulators" in the Prolog DCG or Haskell
    sense.

    Attributes:
        name_ctx: Used to mark ast_cooked.NameNode items as being in a
            binding context (left-hand-side), ref context or raw.  See
            NameCtx for details of these.  It is the responsibility of
            the parent of a node to set this appropriately -- e.g.,
            for an assignment statement, the parent would set name_ctx
            = NameCtx.BINDING for the node(s) to the left of the "="
            and would leave it as name_ctx = NameCtx.REF for node(s)
            on the right. For something like a dotted name on the
            left, the name_ctx would be changed from NameCtx.BINDING
            to NameCtx.REF for all except the last dotted name. The
            normal value for name_ctx is NameCtx.REF; it only becomes
            NameCtx.BINDING on the left-hand side of assignments, for
            parameters in a function definition, and a few other
            similar situations (e.g., a with_item or an
            except_clause). Within import statements, name_ctx can be
            NameCtx.RAW.
        scope_bindings: A set of names that are bindings within this
            "scope". This attribute is set to empty when entering a
            new scope. To ensure consistent results, an OrderedDict
            is used, with the value ignored.
        global_vars: A set of names that appear in "global" statements
            within the current scope.
        nonlocal_vars: A set of names that appear in "nonlocal"
            statements within the current scope.
        python_version: 2 or 3
        src_file: source and offset information
    """

    name_ctx: NameCtx
    scope_bindings: Dict[Text, None]
    global_vars: Dict[Text, None]
    nonlocal_vars: Dict[Text, None]
    python_version: int
    src_file: ast.File
    __slots__ = [
        'name_ctx', 'scope_bindings', 'global_vars', 'nonlocal_vars', 'python_version', 'src_file']

    def __post_init__(self) -> None:
        # scope_bindings should be collections.OrderedDicts if you want
        # deterministic results.
        assert self.python_version in (2, 3)

    def to_BINDING(self) -> 'Ctx':
        return dataclasses.replace(self, name_ctx=NameCtx.BINDING)

    def to_RAW(self) -> 'Ctx':
        return dataclasses.replace(self, name_ctx=NameCtx.RAW)

    def to_REF(self) -> 'Ctx':
        return dataclasses.replace(self, name_ctx=NameCtx.REF)

    @property
    def is_BINDING(self) -> bool:
        return self.name_ctx is NameCtx.BINDING

    @property
    def is_REF(self) -> bool:
        return self.name_ctx is NameCtx.REF


def new_ctx(python_version: int, src_file: ast.File) -> Ctx:
    """Wrapper that creates a new Ctx object."""
    return Ctx(
        name_ctx=NameCtx.REF,
        scope_bindings=collections.OrderedDict(),
        global_vars=collections.OrderedDict(),
        nonlocal_vars=collections.OrderedDict(),
        python_version=python_version,
        src_file=src_file,
    )


def new_ctx_from(ctx: Ctx) -> Ctx:
    """Wrapper that creates a Ctx object for a new scope.

    Keeps the python_version and src_file; all other fields are
    set to their initial value.
    """
    return new_ctx(ctx.python_version, ctx.src_file)


def cvt_annassign(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """annassign: ':' test ['=' test]"""
    # TODO: test case
    assert ctx.is_REF, [node]
    if len(node.children) == 2:
        expr = ast_cooked.OMITTED_NODE
    else:
        expr = cvt(node.children[3], ctx)
    return ast_cooked.RawAnnAssignNode(
        left_annotation=cvt(node.children[1], ctx),
        expr=expr,
    )


def cvt_arglist(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """arglist: argument (',' argument)* [',']"""
    assert ctx.is_REF, [node]
    return ast_cooked.RawArgListNode(args=cvt_children_skip_commas(node, ctx))


def cvt_argument(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    argument: ( test [comp_for] |
                test '=' test |
                '**' expr |
                star_expr )
    """
    assert ctx.is_REF, [node]
    if node.children[0].type == SYMS_TEST:
        if len(node.children) == 1:
            return cvt(node.children[0], ctx)
        if node.children[1].type == token.EQUAL:
            # According to the grammar, the name is a `test`, which
            # should always simplify to a single name, so use cvt() to
            # get that name, and then extract the astn:
            name_cvt = cvt(node.children[0], ctx)
            if isinstance(name_cvt, ast_cooked.NameRefNode):
                return ast_cooked.ArgumentNode(name=name_cvt.name, arg=cvt(node.children[2], ctx))
            logging.warning('argument not in form name=expr: %r', node)  # pragma: no cover
            return cvt(node.children[2], ctx)  # pragma: no cover
        assert node.children[1].type == syms.comp_for, [node]
        assert len(node.children) == 2, [node]
        # the arg is a generator
        return ast_cooked.DictGenListSetMakerCompForNode(
            value_expr=cvt(node.children[0], ctx),
            comp_for=xcast(ast_cooked.CompForNode, cvt(node.children[1], ctx)),
        )
    if node.children[0].type == token.DOUBLESTAR:
        return cvt(node.children[1], ctx)  # Ignore the `**`
    assert node.children[0].type == SYMS_STAR_EXPR, dict(ch0=node.children[0], node=node)
    return cvt(node.children[0], ctx)  # Ignores the `*`


def cvt_assert_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """assert_stmt: 'assert' test [',' test]"""
    assert ctx.is_REF, [node]
    test = cvt(node.children[1], ctx)
    if len(node.children) == 2:
        display = ast_cooked.OMITTED_NODE
    else:
        display = cvt(node.children[3], ctx)
    return ast_cooked.AssertStmt(items=[test, display])


def cvt_async_funcdef(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """async_funcdef: ASYNC funcdef"""
    # TODO: test case
    assert ctx.is_REF, [node]
    return cvt(node.children[1], ctx)  # Ignore the `async`


def cvt_async_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """async_stmt: ASYNC (funcdef | with_stmt | for_stmt)"""
    # TODO: test case
    assert ctx.is_REF, [node]
    return cvt(node.children[1], ctx)  # Ignore the `async`


def cvt_atom(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    atom: ('(' [yield_expr|testlist_gexp] ')' |
           '[' [listmaker] ']' |
           '{' [dictsetmaker] '}' |
           '`' testlist1 '`' |
           NAME | NUMBER | STRING+ | '.' '.' '.')
    """
    # Can appear on left of assignment
    ch0 = node.children[0]
    if ch0.type in _EMPTY_PAIR:
        if len(node.children) == 3:
            result = cvt(node.children[1], ctx)
        else:
            assert len(node.children) == 2, [node]
            if ch0.type == token.LSQB:
                result = ast_cooked.ListMakerNode(items=[], binds=ctx.is_BINDING)
            elif ch0.type == token.LBRACE:
                # TODO: test case to ensure grammar doesn't allow
                #       dictsetmaker on l.h.s. (probaly it does, so
                #       the following assert should become "can't
                #       assign to literal" error).
                assert ctx.is_REF, [node]
                result = ast_cooked.DictSetMakerNode(items=[])
            else:
                result = ast_cooked.ExprListNode(items=[], binds=ctx.is_BINDING)
    elif ch0.type in _CONSTANT:
        result = cvt(ch0, ctx)
    elif (len(node.children) == 3 and
          node.children[0].type == node.children[1].type == node.children[2].type == token.DOT):
        assert ctx.is_REF, [node]
        result = ast_cooked.EllipsisNode()
    else:
        raise ValueError(f'Invalid atom: {node!r}')  # pragma: no cover
    return result


_EMPTY_PAIR = {
    token.LPAR: '()',
    token.LSQB: '[]',
    token.LBRACE: '{}',
    token.BACKQUOTE: '``', }

_CONSTANT = frozenset([token.NAME, token.NUMBER, token.STRING])


def cvt_augassign(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    augassign: ('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
                '<<=' | '>>=' | '**=' | '//=')
    """
    assert ctx.is_REF, [node]
    assert len(node.children) == 1, [node]
    return ast_cooked.AugAssignNode(op=ctx.src_file.astn_to_range(node.children[0]))


def cvt_binary_op(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """Handles the following rules (as modified by _convert()):
       and_expr: shift_expr ('&' shift_expr)*
       and_test: not_test ('and' not_test)*
       arith_expr: term (('+'|'-') term)*
       expr: xor_expr ('|' xor_expr)*
       or_test: and_test ('or' and_test)*
       shift_expr: arith_expr (('<<'|'>>') arith_expr)*
       term: factor (('*'|'@'|'/'|'%'|'//') factor)*
       xor_expr: and_expr ('^' and_expr)*
    """
    result = cvt(node.children[0], ctx)
    if len(node.children) == 1:
        # Can appear on left of assignment if it's a single item;
        # also, this reduces the clutter in the ast_cooked tree
        # without losing any significant information.
        # TODO: modify _EXPR_NODES (used by _convert) to reduce the
        #       raw tree before getting to here
        return result
    assert ctx.is_REF, [node]
    for i in range(1, len(node.children), 2):
        result = ast_cooked.OpNode(
            op_astns=[ctx.src_file.astn_to_range(node.children[i])],
            args=[result, cvt(node.children[i + 1], ctx)],
        )
    return result


def cvt_break_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """break_stmt: 'break'"""
    assert ctx.is_REF, [node]
    return ast_cooked.BreakStmt()


def cvt_classdef(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """classdef: 'class' NAME ['(' [arglist] ')'] ':' suite"""
    assert ctx.is_REF, [node]
    # The bindings for ClassDefStmt are built up in the calls to
    # parameters and suite.
    # TODO: what happens with `def foo(): global Bar; class Bar: ...` ?
    name = xcast(ast_cooked.NameBindsNode, cvt(node.children[1], ctx.to_BINDING()))
    ctx_class = new_ctx_from(ctx)  #  new bindings for parameters, suite
    if node.children[2].type == token.LPAR:
        if node.children[3].type == token.RPAR:
            bases = []  # type: Sequence[ast_cooked.Base]
        else:
            bases = xcast(ast_cooked.RawArgListNode, cvt(node.children[3], ctx_class)).args
    else:
        bases = []
    suite = cvt(node.children[-1], ctx_class)
    return ast_cooked.ClassDefStmt(name=name,
                                   bases=bases,
                                   suite=suite,
                                   scope_bindings=ctx_class.scope_bindings)


def cvt_comp_for(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """comp_for: [ASYNC] 'for' exprlist 'in' test_list_safe [comp_iter]
    """
    assert ctx.is_REF, [node]
    ch0 = xcast(Leaf, node.children[0])
    if ch0.value == 'async':
        # TODO: test case
        children = node.children[1:]  # ignore ASYNC
    else:
        children = node.children
    in_testlist = cvt(children[3], ctx)  # outside the `for`
    ctx_for = (
        ctx if ctx.python_version == 2 else  # TODO: Python 2 test case
        dataclasses.replace(ctx, scope_bindings=collections.OrderedDict()))
    for_exprlist = cvt(children[1], ctx_for.to_BINDING())
    if len(children) == 5:
        comp_iter = cvt(children[4], ctx_for)  # evaluated in context of `for`
    else:
        assert len(children) == 4, [node]
        comp_iter = ast_cooked.OMITTED_NODE
    if ctx.python_version == 2:  # TODO: Python 2 test case
        ctx.scope_bindings.update(ctx_for.scope_bindings)  # pragma: no cover
    return ast_cooked.CompForNode(
        for_astn=ctx.src_file.astn_to_range(children[0]),
        for_exprlist=for_exprlist,
        in_testlist=in_testlist,
        comp_iter=comp_iter,
        scope_bindings=ctx_for.scope_bindings,
    )


def cvt_comp_if(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """comp_if: 'if' old_test [comp_iter]
    """
    assert ctx.is_REF, [node]
    if len(node.children) == 2:
        return cvt(node.children[1], ctx)
    assert len(node.children) == 3, [node]
    return ast_cooked.CompIfCompIterNode(value_expr=cvt(node.children[1], ctx),
                                         comp_iter=cvt(node.children[2], ctx))


def cvt_comp_iter(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """comp_iter: comp_for | comp_if
    """
    assert ctx.is_REF, [node]
    return cvt(node.children[0], ctx)


def cvt_comp_op(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'"""
    assert ctx.is_REF, [node]
    # The following will be replaced in cvt_comparison
    return ast_cooked.OpNode(
        op_astns=[ctx.src_file.astn_to_range(ch) for ch in node.children],
        args=[],
    )


def cvt_comparison(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """comparison: expr (comp_op expr)*"""
    # This is similar to cvt_binary_op
    result = cvt(node.children[0], ctx)
    if len(node.children) == 1:
        # Can appear on left of assignment if it's a single item
        return result
    assert ctx.is_REF, [node]
    for i in range(1, len(node.children), 2):
        op_astns = xcast(ast_cooked.OpNode, cvt(node.children[i], ctx)).op_astns
        typing_debug.assert_all_isinstance(ast.Astn, op_astns)  # TODO: delete
        result = ast_cooked.OpNode(
            op_astns=op_astns,
            args=[result, cvt(node.children[i + 1], ctx)],
        )
    return result


def cvt_compound_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt |
                   funcdef | classdef | decorated | async_stmt
    """
    assert ctx.is_REF, [node]
    return cvt(node.children[0], ctx)


def cvt_continue_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """continue_stmt: 'continue'"""
    assert ctx.is_REF, [node]
    return ast_cooked.ContinueStmt()


def cvt_decorated(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """decorated: decorators (classdef | funcdef | async_funcdef)"""
    assert ctx.is_REF, [node]
    return ast_cooked.DecoratedStmt(items=cvt_children(node, ctx))


def cvt_decorator(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE"""
    assert ctx.is_REF, [node]
    dotted_names = xcast(ast_cooked.DottedNameNode, cvt(node.children[1], ctx.to_RAW()))
    # The grammar says that the dotted_name's are all "raw"; but we
    # treat them like `atom '.' trailer`, where `atom` is always a raw
    # name (see `power: atom trailer*; trailer: ... | '.' NAME`).
    # This is partially handled here and partly by
    # DecoratorDottedNameNode.
    item0 = ast_cooked.NameRefNode(name=dotted_names.items[0].name)
    name = ast_cooked.DecoratorDottedNameNode(items=[item0] + dotted_names.items[1:])
    if node.children[2].type == token.LPAR:
        # TODO: test case
        if node.children[3].type == token.RPAR:
            arglist = []  # type: Sequence[ast_cooked.Base]
        else:
            # TODO: test case
            arglist = xcast(ast_cooked.RawArgListNode, cvt(node.children[3], ctx)).args
    else:
        arglist = []
    return ast_cooked.DecoratorNode(name=name, args=arglist)


def cvt_decorators(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """decorators: decorator+"""
    assert ctx.is_REF, [node]
    return ast_cooked.DecoratorsNode(items=cvt_children(node, ctx))


def cvt_del_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """del_stmt: 'del' exprlist"""
    assert ctx.is_REF, [node]
    exprs = cvt(node.children[1], ctx)
    if isinstance(exprs, ast_cooked.ExprListNode):
        # TODO: test case
        return ast_cooked.DelStmt(items=exprs.items)
    return ast_cooked.DelStmt(items=[exprs])


def cvt_dictsetmaker(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    dictsetmaker: ( ((test ':' test | '**' expr)
                     (comp_for | (',' (test ':' test | '**' expr))* [','])) |
                    ((test | star_expr)
                     (comp_for | (',' (test | star_expr))* [','])) )
    """
    assert ctx.is_REF, [node]
    if len(node.children) == 1:
        return ast_cooked.DictSetMakerNode(items=[cvt(node.children[0], ctx)])
    if (len(node.children) == 4 and node.children[1].type == token.COLON and
            node.children[3].type == syms.comp_for):
        return ast_cooked.DictGenListSetMakerCompForNode(
            value_expr=ast_cooked.DictKeyValue(items=[
                cvt(node.children[0], ctx),
                cvt(node.children[2], ctx), ]),
            comp_for=xcast(ast_cooked.CompForNode, cvt(node.children[3], ctx)),
        )
    if (len(node.children) == 3 and node.children[0].type == token.DOUBLESTAR and
            node.children[2].type == syms.comp_for):
        # TODO: test case
        return ast_cooked.DictGenListSetMakerCompForNode(
            value_expr=cvt(node.children[1], ctx),  # ignore '**'
            comp_for=xcast(ast_cooked.CompForNode, cvt(node.children[2], ctx)))
    if node.children[1] == syms.comp_for:
        # TODO: test case
        assert len(node.children) == 2, [node]
        return ast_cooked.DictGenListSetMakerCompForNode(
            value_expr=cvt(node.children[0], ctx),
            comp_for=xcast(ast_cooked.CompForNode, cvt(node.children[1], ctx)),
        )
    return ast_cooked.DictSetMakerNode(items=[
        cvt(ch, ctx)
        for ch in node.children
        if ch.type not in (token.COLON, token.DOUBLESTAR, token.COMMA)])


def cvt_dotted_as_name(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """dotted_as_name: dotted_name ['as' NAME]"""
    assert ctx.is_REF, [node]
    dotted_name = xcast(ast_cooked.DottedNameNode, cvt(node.children[0], ctx.to_RAW()))
    if len(node.children) == 1:
        # `import os.path` creates a binding for `os`.
        return ast_cooked.ImportDottedAsNameNode(dotted_name=dotted_name, as_name=None)
    as_name = typing.cast(Union[ast_cooked.NameBindsNode, ast_cooked.NameBindsGlobalNode],
                          cvt(node.children[2], ctx.to_BINDING()))
    assert isinstance(
        as_name, (ast_cooked.NameBindsNode, ast_cooked.NameBindsGlobalNode))  # TODO: delete
    return ast_cooked.ImportDottedAsNameNode(dotted_name=dotted_name, as_name=as_name)


def cvt_dotted_as_names(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """dotted_as_names: dotted_as_name (',' dotted_as_name)*"""
    assert ctx.is_REF, [node]
    return ast_cooked.ImportDottedAsNamesNode(items=cvt_children_skip_commas(node, ctx))


def cvt_dotted_name(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """dotted_name: NAME ('.' NAME)*"""
    # If this is on left of assignment, the last name is in a binding context
    return ast_cooked.DottedNameNode(
        items=[cvt(ch, ctx.to_RAW()) for ch in node.children[:-1] if ch.type != token.DOT] +
        [cvt(node.children[-1], ctx)])


def cvt_encoding_decl(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """encoding_decl: NAME

    Used for communication between Python parser and compiler.
    """
    raise ValueError(f'encoding_decl is not used in grammar: {node!r}')  # pragma: no cover


def cvt_eval_input(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """eval_input: testlist NEWLINE* ENDMARKER

    Input for the eval() and input() functions.
    """
    raise ValueError(f'eval_input is not used in grammar: {node!r}')  # pragma: no cover


def cvt_except_clause(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """except_clause: 'except' [test [(',' | 'as') test]]"""
    assert ctx.is_REF, [node]
    if len(node.children) == 1:
        expr = ast_cooked.OMITTED_NODE
        as_item = ast_cooked.OMITTED_NODE
    elif len(node.children) == 2:
        expr = cvt(node.children[1], ctx)
        as_item = ast_cooked.OMITTED_NODE
    else:
        assert len(node.children) == 4, [node]
        expr = cvt(node.children[1], ctx)
        as_item = cvt(node.children[3], ctx.to_BINDING())
    return ast_cooked.ExceptClauseNode(expr=expr, as_item=as_item)


def cvt_exec_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:  # pragma: no cover
    """exec_stmt: 'exec' expr ['in' test [',' test]]"""
    assert ctx.is_REF, [node]
    if len(node.children) == 1:
        expr1 = cvt(node.children[1], ctx)
        expr2 = ast_cooked.OMITTED_NODE
        expr3 = ast_cooked.OMITTED_NODE
    elif len(node.children) == 4:
        expr1 = cvt(node.children[1], ctx)
        expr2 = cvt(node.children[3], ctx)
        expr3 = ast_cooked.OMITTED_NODE
    else:
        assert len(node.children) == 6, [node]
        expr1 = cvt(node.children[1], ctx)
        expr2 = cvt(node.children[3], ctx)
        expr3 = cvt(node.children[5], ctx)
    return ast_cooked.ExecStmt(items=[expr1, expr2, expr3])


def cvt_expr_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    expr_stmt: testlist_star_expr
               ( annassign |
                 augassign (yield_expr|testlist) |
                 ('=' (yield_expr|testlist_star_expr))* )
    """
    assert ctx.is_REF, [node]
    if len(node.children) == 1:
        return ast_cooked.make_stmts([
            ast_cooked.AssignMultipleExprStmt(
                left_list=[],
                expr=cvt(node.children[0], ctx),
            )])
    if len(node.children) == 2:
        # TODO: test case
        assert node.children[1].type == SYMS_ANNASSIGN
        # Treat as binding even if there's no `=` in the annassign,
        # because it's sort of a binding (defines the type).
        annassign = xcast(ast_cooked.RawAnnAssignNode, cvt(node.children[1], ctx))
        return ast_cooked.AnnAssignStmt(left=cvt(node.children[0], ctx.to_BINDING()),
                                        left_annotation=annassign.left_annotation,
                                        expr=annassign.expr)
    if node.children[1].type == token.EQUAL:
        # expr_stmt: testlist_star_expr ('=' (yield_expr|testlist_star_expr))+
        #  (guaranteed at least one ('=' (yield_expr|testlist_star_expr)
        #  because of the test (above): len(node.children) == 1
        expr = cvt(node.children[-1], ctx)
        left_ctx = dataclasses.replace(ctx, name_ctx=NameCtx.BINDING)
        # TODO: (multiple) ast_cooked.AssignExprStmt's (with temporary as needed)
        #       see ast_cooked.AssignMultipleExprStmt.add_fqns
        return ast_cooked.AssignMultipleExprStmt(
            left_list=[
                cvt(ch, left_ctx) for ch in node.children[:-1:2]  # skip '='s
            ],
            expr=expr)
    assert node.children[1].type == SYMS_AUGASSIGN
    augassign = xcast(ast_cooked.AugAssignNode, cvt(node.children[1], ctx))
    expr = cvt(node.children[2], ctx)
    left_augassign = cvt(node.children[0], ctx)  # modifies left; REF context
    return ast_cooked.AugAssignStmt(left=left_augassign, augassign=augassign.op, expr=expr)


def cvt_exprlist(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']"""
    # TODO: Can appear in (LHS) binding context ('for' exprlist ...)?
    #       (or is this only as testlist?)
    return cvt_children_skip_commas_tuple(node, ctx)


def cvt_file_input(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """file_input: (NEWLINE | stmt)* ENDMARKER"""
    assert ctx.is_REF, [node]
    assert all(ch.type in (SYMS_STMT, token.NEWLINE, token.ENDMARKER) for ch in node.children), [
        node]
    stmts = ast_cooked.make_stmts([cvt(ch, ctx) for ch in node.children if ch.type == SYMS_STMT])
    return ast_cooked.FileInput(path=ctx.src_file.path,
                                stmts=stmts.items,
                                scope_bindings=ctx.scope_bindings)


def cvt_flow_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt"""
    assert ctx.is_REF, [node]
    return cvt(node.children[0], ctx)


def cvt_for_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]"""
    assert ctx.is_REF, [node]
    exprlist = cvt(node.children[1], ctx.to_BINDING())
    testlist = cvt(node.children[3], ctx)
    suite = cvt(node.children[5], ctx)
    if len(node.children) == 9:
        else_suite = cvt(node.children[8], ctx)
    else:
        assert len(node.children) == 6, [node]
        else_suite = ast_cooked.OMITTED_NODE
    return ast_cooked.ForStmt(for_exprlist=exprlist,
                              in_testlist=testlist,
                              suite=suite,
                              else_suite=else_suite)


def cvt_funcdef(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """funcdef: 'def' NAME parameters ['->' test] ':' suite"""
    assert ctx.is_REF, [node]
    # The bindings for FuncDefStmt are built up in the calls to
    # parameters and suite.
    name = typing.cast(Union[ast_cooked.NameBindsNode, ast_cooked.NameBindsGlobalNode],
                       cvt(node.children[1], ctx.to_BINDING()))
    assert isinstance(
        name, (ast_cooked.NameBindsNode, ast_cooked.NameBindsGlobalNode))  # TODO: delete
    ctx.scope_bindings[name.name.value] = None
    # start a new set of bindings for the parameters, suite
    ctx_func = new_ctx_from(ctx)
    parameters = xcast(ast_cooked.RawTypedArgsListNode, cvt(node.children[2], ctx_func))
    if node.children[3].type == token.RARROW:
        return_type = cvt(node.children[4], ctx)
    else:
        return_type = ast_cooked.OMITTED_NODE
    suite = cvt(node.children[-1], ctx_func)
    return ast_cooked.FuncDefStmt(name=name,
                                  parameters=parameters.args,
                                  return_type=return_type,
                                  suite=suite,
                                  scope_bindings=ctx_func.scope_bindings)


def cvt_global_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """global_stmt: ('global' | 'nonlocal') NAME (',' NAME)*"""
    assert ctx.is_REF, [node]
    names = [
        xcast(ast_cooked.NameRefNode, cvt(ch, ctx))
        for ch in node.children[1:]
        if ch.type != token.COMMA]
    ch0 = xcast(Leaf, node.children[0])
    if ch0.value == 'global':
        ctx.global_vars.update((name.name.value, None) for name in names)
        return ast_cooked.GlobalStmt(items=names)
    else:
        assert ch0.value == 'nonlocal', [node]
        ctx.nonlocal_vars.update((name.name.value, None) for name in names)
        return ast_cooked.NonLocalStmt(items=names)


def cvt_if_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]"""
    assert ctx.is_REF, [node]
    ifthens = []
    eval_results = []  # TODO: rewrite using functools.reduce or similar
    else_suite = ast_cooked.OMITTED_NODE
    for i in range(0, len(node.children), 4):
        ch0 = xcast(Leaf, node.children[i])
        if ch0.value in ('if', 'elif'):
            ifthens.append(cvt(node.children[i + 1], ctx))
            eval_results.append(fakesys.FAKE_SYS.eval(f'bool({node.children[i + 1]})'))
            assert node.children[i + 2].type == token.COLON, [node]
            ifthens.append(cvt(node.children[i + 3], ctx))
        elif ch0.value == 'else':
            assert node.children[i + 1].type == token.COLON, [node]
            else_suite = cvt(node.children[i + 2], ctx)
    # TODO: if the source redefines `sys`, this won't work, so need to
    #       verify that `sys` is "undefined" (from "import sys", which
    #       is evaluated later).
    return ast_cooked.IfStmt(eval_results=eval_results, items=ifthens + [else_suite])


def cvt_import_as_name(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """import_as_name: NAME ['as' NAME]"""
    assert ctx.is_BINDING, [node]
    ch0 = node.children[0]
    ch0_name = cvt(ch0, ctx.to_RAW())
    if len(node.children) == 1:
        return ast_cooked.AsNameNode(name=ch0_name, as_name=cvt(ch0, ctx.to_BINDING()))
    # TODO: test case
    return ast_cooked.AsNameNode(name=ch0_name, as_name=cvt(node.children[2], ctx.to_BINDING()))


def cvt_import_as_names(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """import_as_names: import_as_name (',' import_as_name)* [',']"""
    assert ctx.is_BINDING, [node]
    return ast_cooked.ImportAsNamesNode(items=cvt_children_skip_commas(node, ctx))


def cvt_import_from(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    import_from: ('from' ('.'* dotted_name | '.'+)
                  'import' ('*' | '(' import_as_names ')' | import_as_names))
    """
    assert ctx.is_REF, [node]
    from_dots = []  # type: List[ast_cooked.Base]
    from_name = None  # type: Optional[ast_cooked.Base]
    for i, child in enumerate(node.children):
        if child.type == token.NAME and xcast(Leaf, child).value == 'from':
            continue
        if child.type == token.NAME and xcast(Leaf, child).value == 'import':
            break
        if child.type == token.DOT:
            # TODO: test case
            from_dots.append(ast_cooked.ImportDotNode(ctx.src_file.astn_to_range(child)))
        else:
            assert not from_name, [node]
            from_name = cvt(child, ctx.to_RAW())
    # pylint: disable=undefined-loop-variable
    assert (node.children[i].type == token.NAME and
            xcast(Leaf, node.children[i]).value == 'import')
    i += 1
    # pylint: enable=undefined-loop-variable
    if node.children[i].type == token.STAR:
        import_part = ast_cooked.StarNode(star=ctx.src_file.astn_to_range(
            node.children[i]))  # type: ast_cooked.Base
    elif node.children[i].type == token.LPAR:
        import_part = cvt(node.children[i + 1], ctx.to_BINDING())
    else:
        import_part = cvt(node.children[i], ctx.to_BINDING())
    return ast_cooked.ImportFromStmt(from_dots=from_dots,
                                     from_name=from_name,
                                     import_part=import_part)


def cvt_import_name(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """import_name: 'import' dotted_as_names"""
    assert ctx.is_REF, [node]
    return ast_cooked.ImportNameNode(dotted_as_names=cvt(node.children[1], ctx))


def cvt_import_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """import_stmt: import_name | import_from"""
    assert ctx.is_REF, [node]
    return cvt(node.children[0], ctx)


def cvt_lambdef(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """lambdef: 'lambda' [varargslist] ':' test"""
    assert ctx.is_REF, [node]
    name = xcast(ast_cooked.NameBindsNode, cvt(node.children[0], ctx.to_BINDING()))
    ctx_func = new_ctx_from(ctx)
    if len(node.children) == 4:
        parameters = xcast(ast_cooked.RawTypedArgsListNode, cvt(node.children[1], ctx_func))
        suite = cvt(node.children[3], ctx_func)
    else:
        parameters = ast_cooked.RawTypedArgsListNode(args=[])
        suite = cvt(node.children[2], ctx_func)
    return ast_cooked.FuncDefStmt(name=name,
                                  parameters=parameters.args,
                                  return_type=ast_cooked.OMITTED_NODE,
                                  suite=suite,
                                  scope_bindings=ctx_func.scope_bindings)


def cvt_listmaker(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """listmaker: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )"""
    # Can appear on l.h.s, e.g. "[dd, mm, yy, tm, tz] = data"
    if len(node.children) > 1 and node.children[1].type == syms.comp_for:
        assert len(node.children) == 2, [node]
        return ast_cooked.DictGenListSetMakerCompForNode(
            value_expr=cvt(node.children[0], ctx),
            comp_for=xcast(ast_cooked.CompForNode, cvt(node.children[1], ctx)),
        )
    return ast_cooked.ListMakerNode(items=cvt_children_skip_commas(node, ctx),
                                    binds=ctx.is_BINDING)


def cvt_parameters(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """parameters: '(' [typedargslist] ')'"""
    assert ctx.is_REF, [node]
    if len(node.children) > 2:
        return cvt(node.children[1], ctx)
    return ast_cooked.RawTypedArgsListNode(args=[])


def cvt_pass_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """pass_stmt: 'pass'"""
    assert ctx.is_REF, [node]
    return ast_cooked.PassStmt()


def cvt_power(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """power: [AWAIT] atom trailer* ['**' factor]"""
    # Can appear on left of assignment

    # lib2to3/Grammar.txt allows many illegal things, such as `a+b =
    # 1` or `foo() = 2. We don't attempt to catch these, so nonsense
    # may result where Python would throw an exception such as
    # "SyntaxError: can't assign to operator" or "SyntaxError: can't
    # assign to function call".
    if (node.children[0].type == token.AWAIT or
        (node.children[0].type == token.NAME and xcast(Leaf, node.children[0]).value == 'await')):
        # TODO: test case
        # ignore AWAIT
        atom_child, *trailer_factor_children = node.children[1:]
    else:
        atom_child, *trailer_factor_children = node.children
    doublestar_factor: Optional[ast_cooked.Base]
    if (len(trailer_factor_children) >= 2 and
            trailer_factor_children[-2].type == token.DOUBLESTAR):
        assert trailer_factor_children[-1].type == SYMS_FACTOR, [node]
        doublestar_factor = cvt(trailer_factor_children[-1], ctx)
        trailer_children = trailer_factor_children[:-2]
    else:
        doublestar_factor = None
        trailer_children = trailer_factor_children
    assert all(ch.type == SYMS_TRAILER for ch in trailer_children[1:]), [node]

    if trailer_children:
        ref_ctx = dataclasses.replace(ctx, name_ctx=NameCtx.REF)
        atom_and_trailer = cvt(atom_child, ref_ctx)
        # all trailer_children get binding of False except for the
        # last one, which gets the current ctx's binding:
        bindings = ((len(trailer_children) - 1) * [False] + [ctx.is_BINDING])
        for ch, binds in zip(trailer_children, bindings):
            trailer = xcast(ast_cooked.BaseAtomTrailer, cvt(ch, ref_ctx))
            atom_and_trailer = trailer.atom_trailer_node(atom_and_trailer, binds)
    else:
        atom_and_trailer = cvt(atom_child, ctx)

    if doublestar_factor:
        return ast_cooked.OpNode(
            op_astns=[ctx.src_file.astn_to_range(node.children[-2])],
            args=[atom_and_trailer, doublestar_factor],
        )
    return atom_and_trailer


def cvt_print_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:  # pragma: no cover
    """
    print_stmt: 'print' ( [ test (',' test)* [','] ] |
                          '>>' test [ (',' test)+ [','] ] )

    For Python2, so there are no test cases
    """
    assert ctx.is_REF, [node]
    return ast_cooked.PrintStmt(items=[
        cvt(ch, ctx) for ch in node.children if ch.type not in (token.COMMA, token.RIGHTSHIFT)])


def cvt_raise_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """raise_stmt: 'raise' [test ['from' test | ',' test [',' test]]]"""
    assert ctx.is_REF, [node]
    if len(node.children) == 1:
        return ast_cooked.RaiseStmt(items=[])
    exc = cvt(node.children[1], ctx)
    if len(node.children) > 2:
        # TODO: test case
        if xcast(Leaf, node.children[2]).value == 'from':
            raise_from = cvt(node.children[3], ctx)
            exc2 = ast_cooked.OMITTED_NODE
            exc3 = ast_cooked.OMITTED_NODE
        else:
            raise_from = ast_cooked.OMITTED_NODE
            exc2 = cvt(node.children[3], ctx)
            if len(node.children) > 3:
                exc2 = cvt(node.children[5], ctx)
            else:
                exc3 = ast_cooked.OMITTED_NODE
    else:
        raise_from = ast_cooked.OMITTED_NODE
        exc2 = ast_cooked.OMITTED_NODE
        exc3 = ast_cooked.OMITTED_NODE
    return ast_cooked.RaiseStmt(items=[exc, exc2, exc3, raise_from])


def cvt_return_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """return_stmt: 'return' [testlist]"""
    assert ctx.is_REF, [node]
    # TODO: the following code just throws away the `return`; in
    #       future, perhaps use this to infer the return type of the
    #       function (if it isn't given by type annotations).
    #       See also cvt_yield_stmt, cvt_yield_expr
    if len(node.children) == 2:
        return cvt(node.children[1], ctx)
    return ast_cooked.OMITTED_NODE


def cvt_simple_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE"""
    # filter for ch.type == SYMS_SMALL_STMT
    assert ctx.is_REF, [node]
    assert all(ch.type in (SYMS_SMALL_STMT, token.SEMI, token.NEWLINE) for ch in node.children), [
        node]
    return ast_cooked.make_stmts(
        cvt(ch, ctx) for ch in node.children if ch.type == SYMS_SMALL_STMT)


def cvt_single_input(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:  # pragma: no cover
    """single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE

    Used by REPL, not for program source.
    """
    raise ValueError(f'single_input is not used in grammar: {node!r}')  # pragma: no cover


def cvt_sliceop(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """sliceop: ':' [test]"""
    # TODO: test case
    assert ctx.is_REF, [node]
    return cvt(node.children[1], ctx)


def cvt_small_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    small_stmt: (expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
                 import_stmt | global_stmt | exec_stmt | assert_stmt)
    """
    assert ctx.is_REF, [node]
    assert len(node.children) == 1, [node]
    return ast_cooked.make_stmts([cvt(node.children[0], ctx)])


def cvt_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """stmt: simple_stmt | compound_stmt"""
    assert ctx.is_REF, [node]
    assert len(node.children) == 1, [node]
    return ast_cooked.make_stmts([cvt(node.children[0], ctx)])


def cvt_subscript(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """subscript: test | [test] ':' [test] [sliceop]"""
    assert ctx.is_REF, [node]
    if len(node.children) == 1:
        if node.children[0].type == token.COLON:
            expr1 = ast_cooked.OMITTED_NODE
        else:
            expr1 = cvt(node.children[0], ctx)
        expr2 = ast_cooked.OMITTED_NODE
        expr3 = ast_cooked.OMITTED_NODE
    else:
        i = 0
        if node.children[i].type == token.COLON:
            expr1 = ast_cooked.OMITTED_NODE
            i += 1
        else:
            expr1 = cvt(node.children[0], ctx)
            i += 2  # skip ':'
        if i < len(node.children):
            if node.children[i].type == SYMS_SLICEOP:
                # TODO: test case
                expr2 = ast_cooked.OMITTED_NODE
            else:
                expr2 = cvt(node.children[i], ctx)
                i += 1
            if i < len(node.children):
                # TODO: test case
                expr3 = cvt(node.children[i], ctx)
            else:
                expr3 = ast_cooked.OMITTED_NODE
        else:
            expr1 = cvt(node.children[0], ctx)
            expr2 = ast_cooked.OMITTED_NODE
            expr3 = ast_cooked.OMITTED_NODE
    return ast_cooked.SubscriptNode(expr1=expr1, expr2=expr2, expr3=expr3)


def cvt_subscriptlist(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """subscriptlist: subscript (',' subscript)* [',']"""
    # Can appear on left of assignment
    return ast_cooked.RawSubscriptListNode(
        subscripts=cvt_children_skip_commas(node, dataclasses.replace(ctx, name_ctx=NameCtx.REF)))


def cvt_suite(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT"""
    assert ctx.is_REF, [node]
    assert all(ch.type in (SYMS_SIMPLE_STMT, SYMS_STMT, token.NEWLINE, token.INDENT, token.DEDENT)
               for ch in node.children), [node]
    return ast_cooked.make_stmts(
        cvt(ch, ctx)
        for ch in node.children
        if ch.type not in (token.NEWLINE, token.INDENT, token.DEDENT))


def cvt_star_expr(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """star_expr: '*' expr"""
    # Can appear on the l.h.s: a,  *b = [1,2,3]
    # or on r.h.s. b=[1,2,3]; x = [0,*b,666]
    # See cvt_testlist_star_expr
    # For now, ignore the `*`
    # TODO: might need to do something else if pykythe.pl needs
    #       to know that this is a list. See Issue #11
    return cvt(node.children[1], ctx)


def cvt_test(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    test: or_test ['if' or_test 'else' test] | lambdef
    old_test: or_test | old_lambdef
    """
    # Can appear on left of assignment
    return cvt(node.children[0], ctx)


def cvt_testlist(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """testlist: test (',' test)* [',']"""
    assert ctx.is_REF, [node]
    return cvt_children_skip_commas_tuple(node, ctx)


def cvt_testlist1(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:  # pragma: no cover
    """testlist1: test (',' test)*

    Python2 only, so there are no test cases
    """
    assert ctx.is_REF, [node]
    return ast_cooked.ExprListNode(items=cvt_children_skip_commas(node, ctx), binds=ctx.is_BINDING)


def cvt_testlist_gexp(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """testlist_gexp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )"""
    # Can appear on left of assignment
    # Similar to cvt_listmaker
    if len(node.children) > 1 and node.children[1].type == syms.comp_for:
        assert len(node.children) == 2, [node]
        return ast_cooked.DictGenListSetMakerCompForNode(
            value_expr=cvt(node.children[0], ctx),
            comp_for=xcast(ast_cooked.CompForNode, cvt(node.children[1], ctx)),
        )
    return cvt_children_skip_commas_tuple(node, ctx)


def cvt_testlist_safe(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """testlist_safe: old_test [(',' old_test)+ [',']]"""
    assert ctx.is_REF, [node]
    return cvt_children_skip_commas_tuple(node, ctx)


def cvt_testlist_star_expr(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']"""
    # Can appear on l.h.s, e.g.:
    #   x, *middle, y = (1, 2, 3, 4, 5)
    # or in some cases on the RHS:
    #   [x, *middle, y]
    # See cvt_star_expr
    return cvt_children_skip_commas_tuple(node, ctx)


def cvt_tfpdef(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    tfpdef: tname | '(' tfplist ')'
    vfpdef: vname | '(' vfplist ')'
    """
    # Can appear on left of assignment
    if len(node.children) == 1:
        return cvt(node.children[0], ctx)
    # TODO: test case
    return cvt(node.children[1], ctx)


def cvt_tfplist(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    tfplist: tfpdef (',' tfpdef)* [',']
    vfplist: vfpdef (',' vfpdef)* [',']
    """
    # TODO: test case
    assert ctx.is_REF, [node]
    return ast_cooked.TfpListNode(items=cvt_children_skip_commas(node, ctx))


def cvt_tname(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    tname: NAME [':' test]
    vname: NAME
    """
    assert ctx.is_BINDING, [node]
    name = cvt(node.children[0], ctx)  # Mark as binds even if no RHS
    if len(node.children) == 1:
        type_expr = ast_cooked.OMITTED_NODE
    else:
        type_expr = cvt(node.children[2], ctx.to_REF())
    return ast_cooked.TnameNode(name=name, type_expr=type_expr)


def cvt_trailer(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME"""
    # Can appear on left of assignment - cvt_power (which contains
    # `atom trailer*`) sets ctx.left_binds appropriately.
    if node.children[0].type == token.LPAR:
        if node.children[1].type == token.RPAR:
            return ast_cooked.RawArgListNode(args=[])
        else:
            return xcast(ast_cooked.RawArgListNode, cvt(node.children[1], ctx.to_REF()))
    if node.children[0].type == token.LSQB:
        return xcast(ast_cooked.RawSubscriptListNode, cvt(node.children[1], ctx.to_REF()))
    assert node.children[0].type == token.DOT, [node]
    return ast_cooked.RawDotNameTrailerNode(name=xcast(
        ast_cooked.NameRawNode,
        cvt(node.children[1], ctx.to_RAW()),
    ))


def cvt_try_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    try_stmt: ('try' ':' suite
               ((except_clause ':' suite)+
                ['else' ':' suite]
                ['finally' ':' suite] |
               'finally' ':' suite))
    """
    assert ctx.is_REF, [node]
    return ast_cooked.TryStmt(
        items=[cvt(ch, ctx) for ch in node.children if ch.type not in (token.COLON, token.NAME)])


def cvt_typedargslist(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """
    typedargslist: ((tfpdef ['=' test] ',')*
                    ('*' [tname] (',' tname ['=' test])* [',' '**' tname] | '**' tname)
                    | tfpdef ['=' test] (',' tfpdef ['=' test])* [','])
    varargslist: ((vfpdef ['=' test] ',')*
                  ('*' [vname] (',' vname ['=' test])*  [',' '**' vname] | '**' vname)
                  | vfpdef ['=' test] (',' vfpdef ['=' test])* [','])
    """
    assert ctx.is_REF, [node]
    i = 0
    args = []
    max_i = len(node.children) - 1
    while i <= max_i:
        ch0 = node.children[i]
        if ch0.type == token.COMMA:
            i += 1
            continue
        if ch0.type in SYMS_TNAMES:  # pylint: disable=no-member
            if i + 1 <= max_i and node.children[i + 1].type == token.EQUAL:
                args.append(
                    ast_cooked.TypedArgNode(
                        tname=xcast(ast_cooked.TnameNode, cvt(ch0, ctx.to_BINDING())),
                        expr=cvt(node.children[i + 2], ctx),
                    ))
                i += 3
            else:
                args.append(
                    ast_cooked.TypedArgNode(
                        tname=xcast(ast_cooked.TnameNode, cvt(ch0, ctx.to_BINDING())),
                        expr=ast_cooked.OMITTED_NODE,
                    ))
                i += 1
        else:
            assert ch0.type in (token.STAR, token.DOUBLESTAR), [i, ch0, node]
            # ignore '*' or '**'
            i += 1
    return ast_cooked.RawTypedArgsListNode(args=args)


def cvt_while_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """while_stmt: 'while' test ':' suite ['else' ':' suite]"""
    assert ctx.is_REF, [node]
    if len(node.children) == 7:
        return ast_cooked.WhileStmt(test=cvt(node.children[1], ctx),
                                    suite=cvt(node.children[3], ctx),
                                    else_suite=cvt(node.children[6], ctx))
    return ast_cooked.WhileStmt(test=cvt(node.children[1], ctx),
                                suite=cvt(node.children[3], ctx),
                                else_suite=ast_cooked.OMITTED_NODE)


def cvt_with_item(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """with_item: test ['as' expr]"""
    assert ctx.is_REF, [node]
    item = cvt(node.children[0], ctx)
    if len(node.children) == 1:
        as_item = ast_cooked.OMITTED_NODE
    else:
        as_item = cvt(node.children[2], ctx.to_BINDING())
    return ast_cooked.WithItemNode(item=item, as_item=as_item)


def cvt_with_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """with_stmt: 'with' with_item (',' with_item)*  ':' suite"""
    assert ctx.is_REF, [node]
    return ast_cooked.WithStmt(items=[
        cvt(ch, ctx) for ch in node.children[1:-2] if ch.type != token.COMMA],
                               suite=cvt(node.children[-1], ctx))


def cvt_with_var(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """with_var: 'as' expr"""
    # TODO: test case
    assert ctx.is_REF, [node]
    return cvt(node.children[1], ctx)


def cvt_yield_expr(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """yield_expr: 'yield' [yield_arg]
    yield_arg: 'from' test | testlist
    """
    # TODO: test cases
    assert ctx.is_REF, [node]
    # Don't care that it's `yield`; just want the expr. See also
    # cvt_return_stmt, cvt_yield_stmt
    # Also, we ignore the `from` if present.
    if len(node.children) == 2:
        ch1 = node.children[1]
        assert ch1.type == syms.yield_arg, [node]
        if len(ch1.children) == 2:
            assert xcast(Leaf, ch1.children[0]).value == 'from', [node]
            return cvt(ch1.children[1], ctx)
        assert len(ch1.children) == 1
        return cvt(ch1.children[0], ctx)
    assert len(node.children) == 1
    return ast_cooked.OMITTED_NODE


def cvt_yield_stmt(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """yield_stmt: yield_expr"""
    # TODO: test case
    # Just throw away `yield`. See also cvt_return_stmt (in future we
    # might want more information)
    assert ctx.is_REF, [node]
    return cvt(node.children[0], ctx)


def cvt_token_name(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """Handle token.NAME."""
    assert isinstance(node, Leaf), [node]
    name_astn = ctx.src_file.astn_to_range(node)
    if ctx.is_BINDING:
        if (node.value not in ctx.global_vars and node.value not in ctx.nonlocal_vars):
            ctx.scope_bindings[node.value] = None
            return ast_cooked.NameBindsNode(name=name_astn)
        return ast_cooked.NameBindsGlobalNode(name=name_astn)
    if ctx.is_REF:
        return ast_cooked.NameRefNode(name=name_astn)
    if ctx.name_ctx is NameCtx.RAW:
        return ast_cooked.NameRawNode(name=name_astn)
    raise ValueError(f'Invalid name_ctx: {ctx.name_ctx} to {node!r}')  # pragma: no cover


def cvt_token_number(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """Handle token.NUMBER."""
    assert ctx.is_REF, [node]
    astn = ctx.src_file.astn_to_range(node)
    if re.fullmatch(tokenize.Imagnumber, node.value):  # type: ignore
        return ast_cooked.NumberComplexNode(astn=astn)
    if re.fullmatch(tokenize.Floatnumber, node.value):  # type: ignore
        return ast_cooked.NumberFloatNode(astn=astn)
    if re.fullmatch(tokenize.Intnumber, node.value):  # type: ignore
        return ast_cooked.NumberIntNode(astn=astn)
    raise ValueError(f'Invalid number: {node!r}')


def cvt_token_string(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """Handle token.NAME."""
    assert ctx.is_REF, [node]
    astns = node if isinstance(node, list) else [node]
    typing_debug.assert_all_isinstance(Leaf, astns)  # TODO: delete
    astn_ranges = [ctx.src_file.astn_to_range(astn) for astn in astns]
    value = xcast(Leaf, astns[0]).value
    if re.match('[^"\']*[bB][^"]*"', value) or re.match("[^'\"]*[bB][^']*'", value):
        return ast_cooked.StringBytesNode(astns=astn_ranges)
    return ast_cooked.StringNode(astns=astn_ranges)


def cvt_unary_op(node: pytree.Base, ctx: Ctx) -> ast_cooked.Base:
    """Handles the following rules (as modified by _convert()):
       factor: ('+'|'-'|'~') factor | power
       not_test: 'not' not_test | comparison
    """
    if len(node.children) == 1:
        # Can appear on left of assignment if it's a single item
        return cvt(node.children[0], ctx)
    assert ctx.is_REF, [node]
    return ast_cooked.OpNode(
        op_astns=[ctx.src_file.astn_to_range(node.children[0])],
        args=[cvt(node.children[1], ctx)],
    )


# The following dispatch table is derived from
# lib2to3.pygram.python_symbols (using lib2to3.pytree._type_reprs). In
# addition, NAME, NUMBER, STRING are added. This is because some
# productions have "test" or similar, which is expected to collapse to
# a name.

# pylint: disable=no-member
_DISPATCH = {
    token.NAME: cvt_token_name,
    token.NUMBER: cvt_token_number,
    token.STRING: cvt_token_string,
    syms.and_expr: cvt_binary_op,
    syms.and_test: cvt_binary_op,
    syms.annassign: cvt_annassign,
    syms.arglist: cvt_arglist,
    syms.argument: cvt_argument,
    syms.arith_expr: cvt_binary_op,
    syms.assert_stmt: cvt_assert_stmt,
    syms.async_funcdef: cvt_async_funcdef,
    syms.async_stmt: cvt_async_stmt,
    syms.atom: cvt_atom,
    syms.augassign: cvt_augassign,
    syms.break_stmt: cvt_break_stmt,
    syms.classdef: cvt_classdef,
    syms.comp_for: cvt_comp_for,
    syms.comp_if: cvt_comp_if,
    syms.comp_iter: cvt_comp_iter,
    syms.comp_op: cvt_comp_op,
    syms.comparison: cvt_comparison,
    syms.compound_stmt: cvt_compound_stmt,
    syms.continue_stmt: cvt_continue_stmt,
    syms.decorated: cvt_decorated,
    syms.decorator: cvt_decorator,
    syms.decorators: cvt_decorators,
    syms.del_stmt: cvt_del_stmt,
    syms.dictsetmaker: cvt_dictsetmaker,
    syms.dotted_as_name: cvt_dotted_as_name,
    syms.dotted_as_names: cvt_dotted_as_names,
    syms.dotted_name: cvt_dotted_name,
    syms.encoding_decl: cvt_encoding_decl,
    syms.eval_input: cvt_eval_input,
    syms.except_clause: cvt_except_clause,
    syms.exec_stmt: cvt_exec_stmt,
    syms.expr: cvt_binary_op,
    syms.expr_stmt: cvt_expr_stmt,
    syms.exprlist: cvt_exprlist,
    syms.factor: cvt_unary_op,
    syms.file_input: cvt_file_input,
    syms.flow_stmt: cvt_flow_stmt,
    syms.for_stmt: cvt_for_stmt,
    syms.funcdef: cvt_funcdef,
    syms.global_stmt: cvt_global_stmt,
    syms.if_stmt: cvt_if_stmt,
    syms.import_as_name: cvt_import_as_name,
    syms.import_as_names: cvt_import_as_names,
    syms.import_from: cvt_import_from,
    syms.import_name: cvt_import_name,
    syms.import_stmt: cvt_import_stmt,
    syms.lambdef: cvt_lambdef,
    syms.listmaker: cvt_listmaker,
    syms.not_test: cvt_unary_op,
    syms.old_lambdef: cvt_lambdef,  # not cvt_old_lambdef
    syms.old_test: cvt_test,  # not cvt_old_test
    syms.or_test: cvt_binary_op,
    syms.parameters: cvt_parameters,
    syms.pass_stmt: cvt_pass_stmt,
    syms.power: cvt_power,
    syms.print_stmt: cvt_print_stmt,
    syms.raise_stmt: cvt_raise_stmt,
    syms.return_stmt: cvt_return_stmt,
    syms.shift_expr: cvt_binary_op,
    syms.simple_stmt: cvt_simple_stmt,
    syms.single_input: cvt_single_input,
    syms.sliceop: cvt_sliceop,
    syms.small_stmt: cvt_small_stmt,
    syms.star_expr: cvt_star_expr,
    syms.stmt: cvt_stmt,
    syms.subscript: cvt_subscript,
    syms.subscriptlist: cvt_subscriptlist,
    syms.suite: cvt_suite,
    syms.term: cvt_binary_op,
    syms.test: cvt_test,
    syms.testlist1: cvt_testlist1,
    syms.testlist: cvt_testlist,
    syms.testlist_gexp: cvt_testlist_gexp,
    syms.testlist_safe: cvt_testlist_safe,
    syms.testlist_star_expr: cvt_testlist_star_expr,
    syms.tfpdef: cvt_tfpdef,
    syms.tfplist: cvt_tfplist,
    syms.tname: cvt_tname,
    syms.trailer: cvt_trailer,
    syms.try_stmt: cvt_try_stmt,
    syms.typedargslist: cvt_typedargslist,
    syms.varargslist: cvt_typedargslist,  # not varargslist
    syms.vfpdef: cvt_tfpdef,  # not vfpdef
    syms.vfplist: cvt_tfplist,  # not vfplist
    syms.vname: cvt_tname,  # not vname
    syms.while_stmt: cvt_while_stmt,
    syms.with_item: cvt_with_item,
    syms.with_stmt: cvt_with_stmt,
    syms.with_var: cvt_with_var,
    syms.xor_expr: cvt_binary_op,
    # syms.yield_arg: cvt_yield_arg,  # handled by cvt_yield_expr
    syms.yield_expr: cvt_yield_expr,
    syms.yield_stmt: cvt_yield_stmt, }

# The following are to prevent pylint complaining about no-member:

SYMS_ANNASSIGN = syms.annassign
SYMS_AUGASSIGN = syms.augassign
SYMS_FACTOR = syms.factor
SYMS_SIMPLE_STMT = syms.simple_stmt
SYMS_SLICEOP = syms.sliceop
SYMS_SMALL_STMT = syms.small_stmt
SYMS_SIMPLE_STMT = syms.simple_stmt
SYMS_STAR_EXPR = syms.star_expr
SYMS_STMT = syms.stmt
SYMS_TEST = syms.test
SYMS_TRAILER = syms.trailer
SYMS_TNAMES = frozenset([syms.tfpdef, syms.vfpdef, syms.tname, syms.vname])

# pylint: enable=no-member

# pylint: disable=dangerous-default-value,invalid-name

# Explanation for the following: https://github.com/python/mypy/issues/4530
_DISPATCH_TYPE = Dict[int, Callable[[Arg(pytree.Base, 'node'
                                        ), Arg(Ctx, 'ctx')],
                                    ast_cooked.Base,
                                   ]]


def cvt(node: pytree.Base, ctx: Ctx, _DISPATCH: _DISPATCH_TYPE = _DISPATCH) -> ast_cooked.Base:
    """Call the appropriate cvt_XXX for node."""
    return _DISPATCH[node.type](node, ctx)


def cvt_debug(node: pytree.Base, ctx: Ctx,
              _DISPATCH: _DISPATCH_TYPE = _DISPATCH) -> ast_cooked.Base:  # pragma: no cover
    """Call the appropriate cvt_XXX for node."""
    # This can be used instead of cvt() for debugging.
    cvt_func = _DISPATCH[node.type]
    try:
        result = cvt_func(node, ctx)
    except Exception as exc:
        raise Exception('%s calling=%s node=%r' % (exc, cvt_func, node)) from exc
    assert isinstance(result, ast_cooked.Base), dict(node=node, result=result)
    return result


def cvt_children(
        node: pytree.Base,  # Node
        ctx: Ctx,
        _DISPATCH: _DISPATCH_TYPE = _DISPATCH) -> Sequence[ast_cooked.Base]:
    """Call the appropriate cvt_XXX for all node.children."""
    return [cvt(ch, ctx) for ch in node.children]


def cvt_children_skip_commas(
        node: pytree.Base,  # Node
        ctx: Ctx,
        _DISPATCH: _DISPATCH_TYPE = _DISPATCH) -> Sequence[ast_cooked.Base]:
    """Call the appropriate cvt_XXX for all node.children that aren't a comma."""
    return [cvt(ch, ctx) for ch in node.children if ch.type != token.COMMA]


def cvt_children_skip_commas_tuple(
        node: pytree.Base,  # Node
        ctx: Ctx,
        _DISPATCH: _DISPATCH_TYPE = _DISPATCH) -> ast_cooked.Base:
    """Like cvt_children_skip_commas, but special case for singleton without comma.

    If node.children is a single item, then just return the result of running `cvt` on it;
    otherwise, return tuple_type with its items being the mapping of `cvt` onto all the
    node.children. This covers the special case of a trailing comma (e.g., `x, = [1]`).
    """
    if len(node.children) == 1:
        return cvt(node.children[0], ctx)
    return ast_cooked.ExprListNode(items=[
        cvt(ch, ctx) for ch in node.children if ch.type != token.COMMA],
                                   binds=ctx.is_BINDING)


# pylint: enable=dangerous-default-value,invalid-name


def parse(src_bytes: bytes, python_version: int) -> pytree.Base:
    """Parse a byte string."""
    # See lib2to3.refactor.RefactoringTool._read_python_source
    # TODO: add detect_encoding to typeshed: lib2to3/pgen2/tokenize.pyi
    # TODO: (non-ascii testcase) 網目錦蛇=1
    with io.BytesIO(src_bytes) as src_f:
        encoding, _ = tokenize.detect_encoding(src_f.readline)  # type: ignore
    src_str = codecs.decode(src_bytes, encoding)
    lib2to3_logger = logging.getLogger('pykythe')
    grammar = pygram.python_grammar
    if python_version == 3:
        # TODO: why doesn't lib2to3.pygram do this for "exec"?
        del grammar.keywords["print"]
        del grammar.keywords["exec"]
    parser_driver = driver.Driver(grammar, convert=_convert, logger=lib2to3_logger)
    if not src_str.endswith('\n'):  # pragma: no cover
        src_str += '\n'  # work around bug in lib2to3
    return parser_driver.parse_string(src_str)


# Node types that get removed if there's only one child. This does not
# include expr, test, yield_expr and a few others ... the intent is to
# reduce the number of AST nodes without increasing the complexity of
# analyzing the AST.
# pylint: disable=no-member
_EXPR_NODES = typing.cast(
    FrozenSet[int],
    frozenset([
        # TODO: uncomment (for performance) -- needs more test cases first:
        # syms.and_expr,
        # syms.and_test,
        # syms.arith_expr,
        # # syms.atom,  # TODO: reinstate?
        # syms.comparison,
        # syms.factor,
        # syms.not_test,
        # syms.old_test,
        # syms.or_test,
        # # syms.power,  # TODO: reinstate?
        # syms.shift_expr,
        # # syms.star_expr,   # Always '*' expr; also needed for call arg
        # syms.term,
        # syms.xor_expr,
        # syms.comp_iter,  # Not an expr, but also not needed
        # syms.compound_stmt,  # Not an expr, but also not needed
    ]))

# pylint: enable=no-member


class Leaf(pytree.Leaf):
    """pytree.Leaf that uses less memory."""
    __slots__ = ['type', 'value', 'context', '_prefix', 'fixers_applied']


class Node(pytree.Node):
    """pytree.Node that uses less memory."""
    __slots__ = ['type', 'children', 'context', 'prefix', 'fixers_applied']


def _convert(grammar: pgen2_grammar.Grammar,
             raw_node: Tuple[int, Text, Tuple[Text, int, int], Optional[List[Union[Node, Leaf]]]]
            ) -> Union[Leaf, Node]:
    """Convert raw node information to a Node or Leaf instance.

    Derived from pytree.convert, by modifying the test for only a
    single child of a node (lib2to3.pytree.convert collapses this to
    the child). [The test collapses nodes with a single child to the
    child; this complicates some of the processing, so instead we only
    collapse some nodes, as specified by _EXPR_NODES.]

    This is passed to the parser driver which calls it whenever a
    reduction of a grammar rule produces a new complete node, so that
    the tree is built strictly bottom-up.
    """
    node_type, value, context, children = raw_node
    if children or node_type in grammar.number2symbol:
        assert isinstance(children, list)  # TODO: delete
        # If there's exactly one child, return that child instead of
        # creating a new node. This is done only for "expr"-type
        # nodes, to reduce the number of nodes that are created (and
        # subsequently processed):

        if len(children) == 1 and node_type in _EXPR_NODES:
            return children[0]
        return Node(node_type, children, context=context)
    else:
        return Leaf(node_type, value, context=context)
