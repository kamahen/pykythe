"""Representation of nodes, for further processing.

ast_raw.cvt traverses an AST (in the lib2to3.pytree format) and puts
it into a more easy to process form. While traversing, it also marks
binding and non-biding uses of all the namnes (including handling of
names that were marked "global" or "nonlocal").

Each node is a subclass of AstNode.
"""

import collections
import logging  # pylint: disable=unused-import
from lib2to3 import pytree
from typing import Any, Dict, Iterator, List, Optional, Sequence, Text, TypeVar, Union
import typing

from . import kythe, pod, typing_debug

# pylint: disable=too-few-public-methods
# pylint: disable-msg=too-many-arguments
# pylint: disable=too-many-lines


class FqnCtx(pod.PlainOldData):
    """Context for computing FQNs (fully qualified names).

    Attributes:
      fqn_dot: The Fully Qualifed Name of this scope
           (module/function/class), followed by a '.'
      bindings: mappings of names to FQNs at this scope
      python_version: 2 or 3
    """

    __slots__ = ('fqn_dot', 'bindings', 'python_version')

    def __init__(
            self,
            *,
            fqn_dot: Text,
            bindings: typing.ChainMap[Text, Text],  # pylint: disable=no-member
            python_version: int) -> None:
        # pylint: disable=super-init-not-called
        self.fqn_dot = fqn_dot
        self.bindings = bindings
        self.python_version = python_version


class AstNode(pod.PlainOldData):
    """Base class for data from AST nodes.

    These correspond to nodes in lib2to3.pytree.{Node,Leaf}.

    This node should not be called directly.
    """

    # TODO: https://github.com/python/mypy/issues/4547
    __slots__ = ()  # type: Sequence[str]

    def __init__(self, **kwargs: Any) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not AstNode, "Must not instantiate AstNode"
        for key, value in kwargs.items():
            setattr(self, key, value)

    def as_json_dict(self) -> Dict[Text, Any]:
        """Recursively turn a node into a dict for JSON-ification."""
        result = collections.OrderedDict(
        )  # type: collections.OrderedDict[Text, Any]
        for k in self.__slots__:
            value = getattr(self, k)
            if value is not None:
                result[k] = _as_json_dict_full(value)
        return {'type': self.__class__.__name__, 'slots': result}

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        """Generate "anchor" nodes for Kythe facts.

        A Kythe "anchor" is a pointer to a piece of source code
        (typically, a "name" of some kind in Python) to which semantic
        information is attached. See AstListExpr for the canonical
        implementation.

        A few nodes are special, such as FuncDefStmt and
        NameNode. These generate fully qualified names (FQNs) that are
        corpus-wide unique and are used for Kythe `ref` and
        `defines/binding`. A FQN is a list of names separated by '.'
        that gives the name hierarchy. There are some examples in the
        tests.

        This code assumes that all subnodes of type NameNode have had
        the `binds` attribute set properly (ast_raw.cvt does this when
        creating each AstNode).

        Arguments:
          ctx: The context for generating the FQN information (mainly
               the FQN of the enclosing scope).
        Returns:
          an iterator of kythe.Anchor items that will be further
          processed to generate Kythe facts.
        """
        raise NotImplementedError(self)


class AstLeafStmt(AstNode):
    """A convenience class for AST nodes (stmts) that have no children."""

    _SelfType = TypeVar('_SelfType', bound='AstLeafStmt')

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class AstListExpr(AstNode):
    """A convenience class for AST nodes (expr) that are a simple list."""

    __slots__ = ('items', )

    def __init__(self, *, items: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not AstListExpr, (
            "Must not instantiate AstListExpr")
        self.items = items

    _SelfType = TypeVar('_SelfType', bound='AstListExpr')

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for item in self.items:
            yield from item.anchors(ctx)


class AstListStmt(AstNode):
    """A convenience class for AST nodes (stmts) that are a simple list."""

    __slots__ = ('items', )

    def __init__(self, *, items: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not AstListStmt, (
            "Must not instantiate AstListStmt")
        self.items = items

    _SelfType = TypeVar('_SelfType', bound='AstListStmt')

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for item in self.items:
            yield from item.anchors(ctx)


def _as_json_dict_full(value: Any) -> Any:
    """Recursively turn an object into a dict for JSON-ification."""
    # pylint: disable=too-many-return-statements
    if isinstance(value, pod.PlainOldData):
        return value.as_json_dict()
    if isinstance(value, list):
        return [_as_json_dict_full(v) for v in value]
    if isinstance(value, pytree.Leaf):
        return {
            'type': 'Leaf',
            'leaf_type': value.type,
            'value': value.value,
            'prefix': value.prefix,
            'lineno': value.lineno,
            'column': value.column
        }
    if isinstance(value, bool):
        return {'type': 'bool', 'value': str(value)},
    if isinstance(value, int):
        return {'type': 'int', 'value': value}
    if isinstance(value, str):
        return {'type': 'str', 'value': value},
    if isinstance(value, dict):
        return {
            'type': 'dict',
            'items': {k: _as_json_dict_full(v)
                      for k, v in value.items()}
        }
    if value is None:
        return {'type': 'None'}
    return {'NOT-POD': value.__class__.__name__, 'value': value}


class AssertStmt(AstListStmt):
    """Corresponds to `assert_stmt`."""


class AnnAssignNode(AstNode):
    """Corresponds to `annassign`.

    TODO: also use for # type: ... comment
    """

    __slots__ = ('expr', 'expr_type')

    def __init__(self, *, expr: AstNode, expr_type: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.expr_type = expr_type

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors(ctx)
        yield from self.expr_type.anchors(ctx)


class ArgListNode(AstNode):
    """Corresponds to `arglist`."""

    __slots__ = ('args', )

    def __init__(self, *, args: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(
            (ArgNode, DictGenListSetMakerCompForNode), args)  # TODO: remove
        self.args = typing.cast(
            Sequence[Union[ArgNode, DictGenListSetMakerCompForNode]], args)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for args_item in self.args:
            yield from args_item.anchors(ctx)


class ArgNode(AstNode):
    """Corresponds to `argument`."""

    __slots__ = ('name_astn', 'arg', 'comp_for')

    def __init__(self, *, name_astn: Optional[pytree.Base], arg: AstNode,
                 comp_for: AstNode) -> None:
        # pylint: disable=super-init-not-called
        assert isinstance(
            name_astn, (pytree.Leaf, type(None))), [name_astn]  # TODO: remove
        self.name_astn = name_astn  # TODO: typing.cast(Optional[pytree.Leaf], name_astn)
        self.arg = arg
        assert isinstance(
            comp_for, (CompForNode, OmittedNode)), [comp_for]  # TODO: remove
        self.comp_for = comp_for  # TODO: typing.cast(Union[CompForNode, OmittedNode], comp_for)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        # TODO: handle self.name_astn (ref to the funcdef)
        comp_for_ctx = self.comp_for.scope_ctx(ctx)
        yield from self.comp_for.anchors(comp_for_ctx)
        yield from self.arg.anchors(comp_for_ctx)


class AsNameNode(AstNode):
    """Corresponds to `import_as_name`."""

    __slots__ = ('name', 'as_name')

    def __init__(self, *, name: AstNode, as_name: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = typing_debug.cast(NameNode, name)
        self.as_name = typing_debug.cast(NameNode, as_name)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors(ctx)
        yield from self.as_name.anchors(ctx)


class AtomTrailerNode(AstNode):
    """Correponds to the atom, trailer part of power."""

    __slots__ = ('atom', 'trailers')

    def __init__(self, *, atom: AstNode, trailers: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.trailers = trailers

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.atom.anchors(ctx)
        for trailers_item in self.trailers:
            yield from trailers_item.anchors(ctx)


class AugAssignNode(AstNode):
    """Corresponds to `augassign`."""

    __slots__ = ('op_astn', )

    def __init__(self, *, op_astn: pytree.Base) -> None:
        # pylint: disable=super-init-not-called
        self.op_astn = typing_debug.cast(pytree.Leaf, op_astn)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class ClassDefStmt(AstNode):
    """Corresponds to `classdef`."""

    __slots__ = ('name', 'bases', 'suite', 'scope_bindings')

    def __init__(self, *, name: AstNode, bases: AstNode, suite: AstNode,
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = typing_debug.cast(NameNode, name)
        assert isinstance(bases, (ArgListNode, OmittedNode))  # TODO: remove
        self.bases = bases  # TODO: typing.cast(Union[ArgListNode, OmittedNode], bases)
        self.suite = suite
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        # TODO: add bases to ClassDefAnchor
        assert self.name.binds
        class_fqn = ctx.fqn_dot + self.name.astn.value
        class_fqn_dot = class_fqn + '.'
        class_ctx = ctx._replace(
            fqn_dot=class_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, class_fqn_dot + name)
                                        for name in self.scope_bindings)))
        yield kythe.ClassDefAnchor(astn=self.name.astn, fqn=class_fqn)
        yield from self.bases.anchors(ctx)
        yield from self.suite.anchors(class_ctx)


class BreakStmt(AstLeafStmt):
    """Corresponds to `break_stmt`."""


class ColonNode(AstListExpr):
    """Corresponds to `test ':' test` in DictGenListSetMakerCompForNode."""


class CompIfCompIterNode(AstNode):
    """Corresponds to `comp_if` with `comp_iter`."""

    __slots__ = ('value_expr', 'comp_iter')

    def __init__(self, *, value_expr: AstNode, comp_iter: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_iter = comp_iter

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.value_expr.anchors(ctx)
        yield from self.comp_iter.anchors(ctx)


class CompForNode(AstNode):
    """Corresponds to `comp_for`."""

    __slots__ = ('astn', 'for_exprlist', 'in_testlist', 'comp_iter',
                 'scope_bindings')

    def __init__(self, *, astn: pytree.Base, for_exprlist: AstNode,
                 in_testlist: AstNode, comp_iter: AstNode,
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.astn = typing_debug.cast(pytree.Leaf, astn)
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.comp_iter = comp_iter
        self.scope_bindings = scope_bindings

    def scope_ctx(self, ctx: FqnCtx) -> FqnCtx:
        """New FqnCtx for the scope of the comp_for (updates ctx for Python2)."""
        if ctx.python_version == 2:
            # The bindings "leak" in Python2
            ctx.bindings.update(
                (name, ctx.fqn_dot + name) for name in self.scope_bindings)
            return ctx
        for_fqn_dot = '{}<comp_for>[{:d},{:d}].'.format(
            ctx.fqn_dot, self.astn.lineno, self.astn.column)
        return ctx._replace(
            fqn_dot=for_fqn_dot,
            bindings=ctx.bindings.new_child(collections.OrderedDict()))

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        # Assume that the caller has created a new child in the
        # bindings, if needed.  This is done at the outermost level of
        # a comp_for (for Python 3), but not for any of the inner
        # comp_for's.
        # This handles the following:
        #    x for x in [1,x]  # `x` in `[1,x]` is outer scope
        #    (x, y) for x in [1,2] for y in range(x)  # `x` in `range(x)` is from `for x`
        # [(x, y) for x in [1,2,x] for y in range(x)]  # error: y undefined
        yield from self.in_testlist.anchors(ctx)
        ctx.bindings.update(
            (name, ctx.fqn_dot + name) for name in self.scope_bindings)
        yield from self.for_exprlist.anchors(ctx)
        yield from self.comp_iter.anchors(ctx)


class CompOpNode(AstNode):
    """Corresponds to `comp_op`."""

    __slots__ = ('op_astns', )

    def __init__(self, *,
                 op_astns: Sequence[Union[pytree.Node, pytree.Leaf]]) -> None:
        # pylint: disable=super-init-not-called
        self.op_astns = op_astns

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class ComparisonOpNode(AstNode):
    """Corresponds to `comparison_op`."""

    __slots__ = ('op', 'args')

    def __init__(self, *, op: AstNode, args: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called,invalid-name
        self.op = typing_debug.cast(CompOpNode, op)
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.op.anchors(ctx)
        for args_item in self.args:
            yield from args_item.anchors(ctx)


class ContinueStmt(AstLeafStmt):
    """Corresponds to `continue_stmt`."""


class DecoratorsNode(AstListExpr):
    """Corresponds to `decorators`."""


class DecoratedStmt(AstListStmt):
    """Corresponds to `decorated`."""


class DecoratorNode(AstNode):
    """Corresponds to `decorator`."""

    __slots__ = ('name', 'arglist')

    def __init__(self, *, name: AstNode, arglist: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = typing_debug.cast(DottedNameNode, name)
        self.arglist = arglist

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors(ctx)
        yield from self.arglist.anchors(ctx)


class DelStmt(AstNode):
    """Corresponds to `del_stmt`."""

    __slots__ = ('exprs', )

    def __init__(self, *, exprs: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.exprs = exprs

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.exprs.anchors(ctx)


class DictSetMakerNode(AstListExpr):
    """Corresponds to `dictsetmaker` without `comp_for`."""


class DictGenListSetMakerCompForNode(AstNode):
    """Corresponds to {`dict_set_maker', `listmaker`, testlist_gexp`} with
    `comp_for`. For our purposes, it's not important to know whether
    this is a list, set, or dict comprehension
    """

    __slots__ = ('value_expr', 'comp_for')

    def __init__(self, *, value_expr: AstNode, comp_for: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_for = typing_debug.cast(CompForNode, comp_for)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        comp_for_ctx = self.comp_for.scope_ctx(ctx)
        yield from self.comp_for.anchors(comp_for_ctx)
        yield from self.value_expr.anchors(comp_for_ctx)


class DotNode(AstNode):
    """Corresponds to a DOT in `import_from`."""

    __slots__ = ()

    def __init__(self) -> None:
        # pylint: disable=super-init-not-called
        pass

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class DotNameTrailerNode(AstNode):
    """Corresponds to '.' NAME in trailer."""

    __slots__ = ('name', )

    def __init__(self, *, name: pytree.Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = typing_debug.cast(pytree.Leaf, name)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []  # TODO: Handled by the atom


class DottedAsNameNode(AstNode):
    """Corresponds to `dotted_as_name`."""

    __slots__ = ('dotted_name', 'as_name')

    def __init__(self, *, dotted_name: AstNode, as_name: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_name = typing_debug.cast(DottedNameNode, dotted_name)
        self.as_name = typing_debug.cast(NameNode, as_name)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.dotted_name.anchors(ctx)
        yield from self.as_name.anchors(ctx)


class DottedAsNamesNode(AstNode):
    """Corresponds to `dotted_as_names`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(DottedAsNameNode,
                                           names)  # TODO: remove
        self.names = typing.cast(Sequence[DottedAsNameNode], names)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors(ctx)


class DottedNameNode(AstNode):
    """Corresponds to `dotted_name`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[NameNode], names)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors(ctx)


class EllipsisNode(AstNode):
    """Corresponds to `...`."""

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class ExecStmt(AstListStmt):
    """Corresponds to `exec_stmt`."""


class ExprListNode(AstNode):
    """Corresponds to `explist`."""

    __slots__ = ('exprs', )

    def __init__(self, *, exprs: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.exprs = exprs

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for exprs_item in self.exprs:
            yield from exprs_item.anchors(ctx)


class ExceptClauseNode(AstNode):
    """Corresponds to `except_clause`."""

    __slots__ = ('expr', 'as_item')

    def __init__(self, *, expr: AstNode, as_item: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.as_item = as_item

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors(ctx)
        yield from self.as_item.anchors(ctx)


class ExprStmt(AstNode):
    """Corresponds to `expr_stmt`."""

    __slots__ = ('lhs', 'augassign', 'exprs')

    def __init__(self, *, lhs: AstNode, augassign: AstNode,
                 exprs: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        assert isinstance(augassign,
                          (AugAssignNode, OmittedNode))  # TODO: remove
        # TODO: self.augassign = typing.cast(Union[AugAssignNode, OmittedNode], augassign)
        self.augassign = augassign
        self.exprs = exprs

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.lhs.anchors(ctx)
        yield from self.augassign.anchors(ctx)
        for exprs_item in self.exprs:
            yield from exprs_item.anchors(ctx)


class FileInput(AstNode):
    """Corresponds to `file_input`."""

    __slots__ = ('stmts', 'scope_bindings')

    def __init__(self, *, stmts: Sequence[AstNode],
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.stmts = stmts
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        file_ctx = ctx._replace(
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, ctx.fqn_dot + name)
                                        for name in self.scope_bindings)))
        for stmt in self.stmts:
            yield from stmt.anchors(file_ctx)


class ForStmt(AstNode):
    """Corresponds to `for_stmt`."""

    __slots__ = ('exprlist', 'testlist', 'suite', 'else_suite')

    def __init__(self, *, exprlist: AstNode, testlist: AstNode, suite: AstNode,
                 else_suite: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.exprlist = exprlist
        self.testlist = testlist
        self.suite = suite
        self.else_suite = else_suite

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        exprlist_anchors = self.exprlist.anchors(ctx))  # Adds to ctx
        testlist_anchors = self.testlist.anchors(ctx))
        yield from exprlist_anchors
        yield from testlist_anchors
        yield from self.suite.anchors(ctx)
        yield from self.else_suite.anchors(ctx)


class FuncDefStmt(AstNode):
    """Corresponds to `funcdef` / `async_funcdef` or lambdadef.

    If it's a lambda, the `name` points to the `lambda` keyword.
    """

    __slots__ = ('name', 'parameters', 'return_type', 'suite',
                 'scope_bindings')

    def __init__(self, *, name: 'NameNode', parameters: Sequence[AstNode],
                 return_type: AstNode, suite: AstNode,
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.parameters = parameters
        self.return_type = return_type
        self.suite = suite
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        assert self.name.binds
        # '.<local>.' is needed to distinguish `x` in following:
        #    def foo(x): pass
        #    foo.x = 'a string'
        if self.name.astn.value == 'lambda':
            # Make a unique name for the lambda
            func_fqn = '{}<lambda>[{:d},{:d}]'.format(
                ctx.fqn_dot, self.name.astn.lineno, self.name.astn.column)
        else:
            func_fqn = '{}{}'.format(ctx.fqn_dot, self.name.astn.value)
        func_fqn_dot = func_fqn + '.<local>.'
        func_ctx = ctx._replace(
            fqn_dot=func_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, func_fqn_dot + name)
                                        for name in self.scope_bindings)))
        yield kythe.FuncDefAnchor(astn=self.name.astn, fqn=func_fqn)
        yield from self.return_type.anchors(func_ctx)
        for parameter in self.parameters:
            yield from parameter.anchors(func_ctx)
        yield from self.suite.anchors(func_ctx)


class GlobalStmt(AstNode):
    """Corresponds to `global_stmt`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[NameNode], names)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors(ctx)


class IfStmt(AstListStmt):
    """Corresponds to `if_stmt`."""


class ImportAsNamesNode(AstNode):
    """Corresponds to `import_as_names`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(AsNameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[AsNameNode], names)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors(ctx)


class ImportFromStmt(AstNode):
    """Corresponds to `import_name`."""

    __slots__ = ('from_name', 'import_part')

    def __init__(self, *, from_name: Sequence[AstNode],
                 import_part: AstNode) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(DottedNameNode,
                                           from_name)  # TODO: remove
        self.from_name = typing.cast(Sequence[DottedNameNode], from_name)
        assert isinstance(import_part,
                          (ImportAsNamesNode, StarNode))  # TODO: remove
        # TODO: self.import_part = typing.cast(Union[ImportAsNamesNode, StarNode], import_part)
        self.import_part = import_part

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for from_name_item in self.from_name:
            yield from from_name_item.anchors(ctx)
        yield from self.import_part.anchors(ctx)


class ImportNameNode(AstNode):
    """Corresponds to `import_name`."""

    __slots__ = ('dotted_as_names', )

    def __init__(self, *, dotted_as_names: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_as_names = typing_debug.cast(DottedAsNamesNode,
                                                 dotted_as_names)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.dotted_as_names.anchors(ctx)


class ListMakerNode(AstListExpr):
    """Corresponds to `listmaker` without `comp_for`."""


class NameNode(AstNode):
    """Corresponds to a NAME node.

    Attributes:
        binds: Whether this name is in a binding context or not.
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    __slots__ = ('binds', 'astn')

    def __init__(self, *, binds: bool, astn: pytree.Leaf) -> None:
        # pylint: disable=super-init-not-called
        self.binds = binds
        self.astn = astn

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        name = self.astn.value
        if name in ctx.bindings:
            fqn = ctx.bindings[name]
        else:
            fqn = ctx.fqn_dot + self.astn.value
            ctx.bindings[name] = fqn
        if fqn:  # There are some obscure cases where fqn doesn't get
            # filled in, typically due to the grammar accepting an
            # illegal Python program (e.g., the grammar allows
            # test=test for an arg, but it should be NAME=test)
            if self.binds:
                yield kythe.BindingAnchor(astn=self.astn, fqn=fqn)
            else:
                yield kythe.RefAnchor(astn=self.astn, fqn=fqn)


class NonLocalStmt(AstNode):
    """Corresponds to "nonlocal" variant of `global_stmt`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[NameNode], names)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors(ctx)


class NumberNode(AstNode):
    """Corresponds to a NUMBER node.

    Attributes:
    astn: The AST node of the number
    """

    __slots__ = ('astn', )

    def __init__(self, *, astn: pytree.Leaf) -> None:
        # pylint: disable=super-init-not-called
        self.astn = astn

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class OpNode(AstNode):
    """Corresponds to various expression nodes (unary, binary)."""

    __slots__ = ('op_astn', 'args')

    def __init__(self, *, op_astn: pytree.Base,
                 args: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.op_astn = typing_debug.cast(pytree.Leaf, op_astn)
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for args_item in self.args:
            yield from args_item.anchors(ctx)


class PassStmt(AstLeafStmt):
    """Corresponds to `pass_stmt`."""


class PrintStmt(AstListStmt):
    """Corresponds to `print_stmt`."""


class RaiseStmt(AstListStmt):
    """Corresponds to `raise_stmt`."""


class StarExprNode(AstNode):
    """Corresponds to `star_expr`."""

    __slots__ = ('expr', )

    def __init__(self, *, expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors(ctx)


class StarStarExprNode(AstNode):
    """Corresponds to `'**' expr`."""

    __slots__ = ('expr', )

    def __init__(self, *, expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors(ctx)


class Stmts(AstListStmt):
    """Corresponds to `simple_stmt`, `suite`."""


class StringNode(AstNode):
    """Corresponds to a STRING node.

    Attributes:
        astns: The AST nodes of the string
    """

    __slots__ = ('astns', )

    def __init__(self, *, astns: Sequence[pytree.Leaf]) -> None:
        # pylint: disable=super-init-not-called
        self.astns = astns

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class SubscriptListNode(AstNode):
    """Corresponds to `subscript_list`."""

    __slots__ = ('subscripts', )

    def __init__(self, *, subscripts: Sequence[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(SubscriptNode,
                                           subscripts)  # TODO: remove
        self.subscripts = typing.cast(Sequence[SubscriptNode], subscripts)

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for subscripts_item in self.subscripts:
            yield from subscripts_item.anchors(ctx)


class SubscriptNode(AstNode):
    """Corresponds to `subscript`."""

    __slots__ = ('expr1', 'expr2', 'expr3')

    def __init__(self, *, expr1: AstNode, expr2: AstNode,
                 expr3: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.expr1.anchors(ctx)
        yield from self.expr2.anchors(ctx)
        yield from self.expr3.anchors(ctx)


class TestListNode(AstListExpr):
    """Corresponds to `testlist`, `testlist1`, `testlist_gexp`
    `testlist_star_expr` without `comp_for`."""


class TnameNode(AstNode):
    """Corresponds to `tname`."""

    __slots__ = ('name', 'type_expr')

    def __init__(self, *, name: AstNode, type_expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = typing_debug.cast(NameNode, name)
        self.type_expr = type_expr

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors(ctx)
        yield from self.type_expr.anchors(ctx)


class TfpListNode(AstListExpr):
    """Corresponds to `tfplist`."""


class TryStmt(AstListStmt):
    """Corresponds to `try_stmt`."""


class TypedArgNode(AstNode):
    """Corresponds to `typed_arg`."""

    __slots__ = ('name', 'expr')

    def __init__(self, *, name: AstNode, expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = typing_debug.cast(TnameNode, name)
        self.expr = expr

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors(ctx)
        yield from self.expr.anchors(ctx)


class TypedArgsListNode(AstNode):
    """Corresponds to `typedargslist`.

    This is only used when processing a funcdef; the args are given
    directly to FuncDefStmt, which is why anchors() isn't
    defined for TypedArgsListNode.
    """

    __slots__ = ('args', )

    def __init__(self, *, args: Sequence[TypedArgNode]) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        # Not used anywhere
        raise NotImplementedError(self)


class WhileStmt(AstListStmt):
    """Corresponds to `while_stmt`."""

    __slots__ = ('test', 'suite', 'else_suite')

    def __init__(self, *, test: AstNode, suite: AstNode,
                 else_suite: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.test = test
        self.suite = suite
        self.else_suite = else_suite

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.test.anchors(ctx)
        yield from self.suite.anchors(ctx)
        yield from self.else_suite.anchors(ctx)


class WithItemNode(AstNode):
    """Corresponds to `with_item`."""

    __slots__ = ('item', 'as_item')

    def __init__(self, *, item: AstNode, as_item: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.item = typing_debug.cast(AtomTrailerNode, item)
        self.as_item = as_item

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from self.item.anchors(ctx)
        yield from self.as_item.anchors(ctx)


class WithStmt(AstNode):
    """Corresponds to `with_stmt`."""

    __slots__ = ('items', 'suite')

    def __init__(self, *, items: Sequence[AstNode], suite: AstNode) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(WithItemNode, items)  # TODO: remove
        self.items = typing.cast(Sequence[WithItemNode], items)
        self.suite = suite

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        for items_item in self.items:
            yield from items_item.anchors(ctx)
        yield from self.suite.anchors(ctx)


class YieldNode(AstListExpr):
    """Corresponds to `yield_expr`."""


class OmittedNode(AstNode):
    """An item that is omitted (e.g., bases for a class)."""

    __slots__ = ()

    def scope_ctx(self, ctx: FqnCtx) -> FqnCtx:  # pylint: disable=no-self-use
        # For nodes that can be Union[CompForNode, OmittedNode]
        return ctx

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


class StarNode(AstNode):
    """Corresponds to `'*' expr`."""

    __slots__ = ()

    def anchors(self, ctx: FqnCtx) -> Iterator[kythe.Anchor]:
        yield from []


# Singleton OmittedNode, to avoid creating many of them.
OMITTED_NODE = OmittedNode()
