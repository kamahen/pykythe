"""Representation of nodes, for further processing.

ast_raw.cvt traverses an AST (in the lib2to3.pytree format) and puts
it into a more easy to process form. While traversing, it also marks
binding and non-biding uses of all the namnes (including handling of
names that were marked "global" or "nonlocal").

Each node is a subclass of Base.
"""

import collections
import logging  # pylint: disable=unused-import
from lib2to3 import pytree
from typing import Any, Callable, Dict, List, Optional, Sequence, Text, TypeVar, Union  # pylint: disable=unused-import
import typing
from mypy_extensions import Arg  # pylint: disable=unused-import

from . import ast_fqn, kythe, pod, typing_debug
from .typing_debug import cast as xcast

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


class Base(pod.PlainOldDataExtended):
    """Base class for data from AST nodes.

    These correspond to nodes in lib2to3.pytree.{Node,Leaf}.

    This node should not be called directly.
    """

    # TODO: https://github.com/python/mypy/issues/4547
    __slots__ = ()  # type: Sequence[str]

    def __init__(self, **kwargs: Any) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not Base, "Must not instantiate ast_cooked.Node"
        for key, value in kwargs.items():
            setattr(self, key, value)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        """Generate "anchor" nodes for Kythe facts.

        A Kythe "anchor" is a pointer to a piece of source code
        (typically, a "name" of some kind in Python) to which semantic
        information is attached. See ListExprBase for the canonical
        implementation.

        A few nodes are special, such as FuncDefStmt and
        NameNode. These generate fully qualified names (FQNs) that are
        corpus-wide unique and are used for Kythe `ref` and
        `defines/binding`. A FQN is a list of names separated by '.'
        that gives the name hierarchy. There are some examples in the
        tests.

        This code assumes that all subnodes of type NameNode have had
        the `binds` attribute set properly (ast_raw.cvt does this when
        creating each Node).

        The code is a bit ugly -- it updates one of the parameters
        ("anchors") by appending to it, and it also returns a
        value. Think of this as an optimization; if we were
        programming in Haskell or Prolog, we'd use a linked-list for
        anchors, which is O(1) for cons-ing; but for Python it's O(n)
        and making a linked-list has its own overheads.

        Arguments:
          ctx: The context for generating the FQN information (mainly
               the FQN of the enclosing scope).
          anchors: A list that is appeneded with kythe.Anchor items
               that will be further processed to generate Kythe facts.
               In effect, this is a return value (it could be done with
               "yield" statements, but that precludes returning multiple
               values).

        Returns:
          If the node is an "expression", then an ast_fqn.Base that
          captures the essential information for determining the type
          of the expression. If the node is a "statement", then returns
          None.
        """
        # TODO: Also, we should have two classes -- one for statements
        #       which don't return anything) and one for expressions
        #       (which do).
        raise NotImplementedError(self)

    def anchors_expr(self, ctx: FqnCtx,
                     anchors: List[kythe.Anchor]) -> ast_fqn.Base:
        """Generate "anchor" nodes for Kythe facts, with ast_fqn.Base tree."""
        result = self.anchors(ctx, anchors)
        # assert result is not None  # implicitly done by th excast:
        return xcast(ast_fqn.Base, result)


class LeafStmtBase(Base):
    """A convenience class for AST nodes (stmts) that have no children."""

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return None


class ListExprBase(Base):
    """A convenience class for AST nodes (expr) that are a simple list."""

    __slots__ = ('items', )

    # result_type = None  # type: ast_fqn.ListBase
    result_type = None  # type: Callable[[Arg(Sequence[ast_fqn.Base], 'items')], ast_fqn.ListBase]

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not ListExprBase, (
            "Must not instantiate ast_cooked.ListExprBase")
        self.items = items

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        items_expr = []  # type: List[ast_fqn.Base]
        for item in self.items:
            items_expr.append(item.anchors_expr(ctx, anchors))
        return self.result_type(items=items_expr)  # type: ignore  # pylint: disable=not-callable


class ListStmtBase(Base):
    """A convenience class for AST nodes (stmts) that are a simple list."""

    __slots__ = ('items', )

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not ListStmtBase, (
            "Must not instantiate ast_cooked.ListStmtBase")
        self.items = items

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for item in self.items:
            # TODO: remove the try/except (it's for debugging)
            try:
                item.anchors(ctx, anchors)
            except AssertionError as exc:
                raise RuntimeError('%r: %r' % (exc, item))
        return None


class AnnAssignNode(Base):
    """Corresponds to `annassign` (expr can be OmittedNode).

    This is only used when processing an annassign statement;
    ast_raw.cvt_expr_stmt directly uses the contents of this node.
    """

    # TODO: also use for # type: ... comment

    __slots__ = ('lhs_type', 'expr')

    def __init__(self, *, lhs_type: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs_type = lhs_type
        self.expr = expr

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        # Not used anywhere
        raise NotImplementedError(self)


class AnnAssignStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr annassign`.

    expr can be OmittedNode
    """

    __slots__ = ('lhs', 'lhs_type', 'expr')

    def __init__(self, *, lhs: Base, lhs_type: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.lhs_type = lhs_type
        self.expr = expr

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        lhs_type_expr = self.lhs_type.anchors_expr(ctx, anchors)
        expr_expr = self.expr.anchors_expr(ctx, anchors)
        lhs_expr = self.lhs.anchors_expr(ctx, anchors)
        return ast_fqn.AnnAssign(
            lhs=lhs_expr, lhs_type=lhs_type_expr, expr=expr_expr)


class ArgListNode(Base):
    """Corresponds to `arglist`.

    The grammar uses this for `decorator` and `classdef`, but for
    DecoratorNode and ClassDefNode, we only store the list of
    args. This leave ArgListNode only used by `trailers` (and
    therefore by `power`).
    """

    __slots__ = ('args', )

    def __init__(self, *, args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        args_expr = []  # type: List[ast_fqn.Base]
        for args_item in self.args:
            args_expr.append(args_item.anchors_expr(ctx, anchors))
        return ast_fqn.ArgList(items=args_expr)


class ArgumentNode(Base):
    """Corresponds to `argument: test '=' test`."""

    __slots__ = ('name_astn', 'arg')

    def __init__(self, *, name_astn: pytree.Base, arg: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name_astn = xcast(pytree.Leaf, name_astn)
        self.arg = arg

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        # TODO: handle self.name_astn (ref to the funcdef)
        arg_expr = self.arg.anchors_expr(ctx, anchors)
        return arg_expr


class AsNameNode(Base):
    """Corresponds to `import_as_name`."""

    __slots__ = ('name', 'as_name')

    def __init__(self, *, name: Base, as_name: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(NameNode, name)
        self.as_name = xcast(NameNode, as_name)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        # TODO: output anchor for self.name
        self.as_name.anchors(ctx, anchors)
        return ast_fqn.AsName(
            name=xcast(NameNode, self.name).astn.value,
            as_name=xcast(NameNode, self.as_name).astn.value)


class AssignExprStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr ('=' (yield_expr|testlist_star_expr))*`.

    lhs is a list of assignment bindings; it can be empty.
    """

    __slots__ = ('lhs', 'expr')

    def __init__(self, *, lhs: Sequence[Base], expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.expr = expr

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        expr_expr = self.expr.anchors_expr(ctx, anchors)
        lhs_expr = []  # List[ast_fqn.Base]
        for lhs_item in self.lhs:  # even though assigned in reverse order
            lhs_expr.append(lhs_item.anchors_expr(ctx, anchors))
        return ast_fqn.Assign(lhs=lhs_expr, expr=expr_expr)


class AssertStmt(ListStmtBase):
    """Corresponds to `assert_stmt`."""


class AtomTrailerNode(Base):
    """Correponds to the atom, trailer part of power."""

    __slots__ = ('atom', 'trailers')

    def __init__(self, *, atom: Base, trailers: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.trailers = trailers

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        atom_expr = self.atom.anchors_expr(ctx, anchors)
        trailers_expr = []  # type: List[ast_fqn.Base]
        for trailers_item in self.trailers:
            trailers_expr.append(trailers_item.anchors_expr(ctx, anchors))
        return ast_fqn.AtomTrailer(atom=atom_expr, trailers=trailers_expr)


class AugAssignNode(Base):
    """Corresponds to `augassign`."""

    __slots__ = ('op_astn', )

    def __init__(self, *, op_astn: pytree.Base) -> None:
        # pylint: disable=super-init-not-called
        self.op_astn = xcast(pytree.Leaf, op_astn)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        # Not used anywhere
        raise NotImplementedError(self)


class AugAssignStmt(Base):
    """Corresponds to expr_stmt: augassign (yield_expr|testlist)."""

    __slots__ = ('lhs', 'augassign', 'expr')

    def __init__(self, *, lhs: Base, augassign: pytree.Base,
                 expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.augassign = augassign
        self.expr = expr

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        expr_expr = self.expr.anchors_expr(ctx, anchors)
        lhs_expr = self.lhs.anchors_expr(ctx, anchors)
        return ast_fqn.AugAssign(lhs=lhs_expr, expr=expr_expr)


class BreakStmt(LeafStmtBase):
    """Corresponds to `break_stmt`."""


class ClassDefStmt(Base):
    """Corresponds to `classdef`."""

    __slots__ = ('name', 'bases', 'suite', 'scope_bindings')

    def __init__(self, *, name: Base, bases: Sequence[Base], suite: Base,
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(NameNode, name)
        self.bases = bases
        self.suite = suite
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        assert self.name.binds
        class_fqn = ctx.fqn_dot + self.name.astn.value
        class_fqn_dot = class_fqn + '.'
        class_ctx = ctx._replace(
            fqn_dot=class_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, class_fqn_dot + name)
                                        for name in self.scope_bindings)))
        bases_expr = []  # type: List[ast_fqn.Base]
        for base in self.bases:
            bases_expr.append(base.anchors_expr(ctx, anchors))
        anchors.append(
            kythe.ClassDefAnchor(
                astn=self.name.astn, fqn=class_fqn, bases=bases_expr))
        self.suite.anchors(class_ctx, anchors)
        return None


class ColonNode(ListExprBase):
    """Corresponds to `test ':' test` in DictGenListSetMakerCompForNode."""

    result_type = ast_fqn.Colon


class ComparisonNode(ListExprBase):
    """Corresponds to `comparison`."""

    result_type = ast_fqn.Comparison


class CompForNode(Base):
    """Corresponds to `comp_for`."""

    __slots__ = ('astn', 'for_exprlist', 'in_testlist', 'comp_iter',
                 'scope_bindings')

    def __init__(self, *, astn: pytree.Base, for_exprlist: Base,
                 in_testlist: Base, comp_iter: Base,
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.astn = xcast(pytree.Leaf, astn)
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

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        # Assume that the caller has created a new child in the
        # bindings, if needed.  This is done at the outermost level of
        # a comp_for (for Python 3), but not for any of the inner
        # comp_for's.
        # This handles the following:
        #    x for x in [1,x]  # `x` in `[1,x]` is outer scope
        #    (x, y) for x in [1,2] for y in range(x)  # `x` in `range(x)` is from `for x`
        # [(x, y) for x in [1,2,x] for y in range(x)]  # error: y undefined
        in_testlist_expr = self.in_testlist.anchors_expr(ctx, anchors)
        ctx.bindings.update(
            (name, ctx.fqn_dot + name) for name in self.scope_bindings)
        for_exprlist_expr = self.for_exprlist.anchors_expr(ctx, anchors)
        comp_iter_expr = self.comp_iter.anchors_expr(ctx, anchors)
        return ast_fqn.CompFor(
            for_exprlist=for_exprlist_expr,
            in_testlist=in_testlist_expr,
            comp_iter=comp_iter_expr)


class CompIfCompIterNode(Base):
    """Corresponds to `comp_if` with `comp_iter`."""

    __slots__ = ('value_expr', 'comp_iter')

    def __init__(self, *, value_expr: Base, comp_iter: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_iter = comp_iter

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        value_expr_expr = self.value_expr.anchors_expr(ctx, anchors)
        comp_iter_expr = self.comp_iter.anchors_expr(ctx, anchors)
        return ast_fqn.CompIfCompIter(
            value_expr=value_expr_expr, comp_iter=comp_iter_expr)


class ContinueStmt(LeafStmtBase):
    """Corresponds to `continue_stmt`."""


class DecoratorsNode(ListExprBase):
    """Corresponds to `decorators`."""

    result_type = ast_fqn.Decorators


class DecoratedStmt(ListStmtBase):
    """Corresponds to `decorated`."""


class DecoratorNode(Base):
    """Corresponds to `decorator`."""

    __slots__ = ('name', 'arglist')

    def __init__(self, *, name: Base, arglist: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(DottedNameNode, name)
        self.arglist = arglist

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        name_expr = self.name.anchors_expr(ctx, anchors)
        trailers_expr = []  # List[ast_fqn.Base]
        for arg in self.arglist:
            trailers_expr.append(arg.anchors_expr(ctx, anchors))
        return ast_fqn.AtomTrailer(atom=name_expr, trailers=trailers_expr)


class DelStmt(Base):
    """Corresponds to `del_stmt`."""

    __slots__ = ('exprs', )

    def __init__(self, *, exprs: Base) -> None:
        # pylint: disable=super-init-not-called
        self.exprs = exprs

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.exprs.anchors(ctx, anchors)
        return None


class DictSetMakerNode(ListExprBase):
    """Corresponds to `dictsetmaker` without `comp_for`."""

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return ast_fqn.Dict(value=ast_fqn.Union(
            items=[]))  # TODO: union the types of the values


class DictGenListSetMakerCompForNode(Base):
    """Corresponds to {`dict_set_maker', `listmaker`, testlist_gexp`} with
    `comp_for`. For our purposes, it's not important to know whether
    this is a list, set, or dict comprehension
    """

    __slots__ = ('value_expr', 'comp_for')

    def __init__(self, *, value_expr: Base, comp_for: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_for = xcast(CompForNode, comp_for)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        comp_for_ctx = self.comp_for.scope_ctx(ctx)
        self.comp_for.anchors(comp_for_ctx, anchors)
        value_expr = self.value_expr.anchors_expr(comp_for_ctx, anchors)
        return ast_fqn.Dict(value=value_expr)


class DotNode(Base):
    """Corresponds to a DOT in `import_from`."""

    __slots__ = ()

    def __init__(self) -> None:
        # pylint: disable=super-init-not-called
        pass

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return None


class DotNameTrailerNode(Base):
    """Corresponds to '.' NAME in trailer."""

    __slots__ = ('name', )

    def __init__(self, *, name: pytree.Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(pytree.Leaf, name)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return ast_fqn.Dot(name=self.name.value)


class DottedAsNameNode(Base):
    """Corresponds to `dotted_as_name`."""

    __slots__ = ('dotted_name', 'as_name')

    def __init__(self, *, dotted_name: Base, as_name: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_name = xcast(DottedNameNode, dotted_name)
        self.as_name = xcast(NameNode, as_name)
        assert self.as_name.binds

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.dotted_name.anchors(ctx, anchors)
        self.as_name.anchors(ctx, anchors)
        return None


class DottedAsNamesNode(Base):
    """Corresponds to `dotted_as_names`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(DottedAsNameNode,
                                           names)  # TODO: remove
        self.names = typing.cast(Sequence[DottedAsNameNode], names)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for names_item in self.names:
            names_item.anchors(ctx, anchors)
        return None


class DottedNameNode(Base):
    """Corresponds to `dotted_name`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[NameNode], names)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for names_item in self.names:
            names_item.anchors(ctx, anchors)
        return ast_fqn.DottedName(
            names=[name.astn.value for name in self.names])


class EllipsisNode(Base):
    """Corresponds to `...`."""

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return ast_fqn.EllipsisConst()


class ExecStmt(ListStmtBase):
    """Corresponds to `exec_stmt`."""


class ExprListNode(Base):
    """Corresponds to `exprlist`."""

    __slots__ = ('exprs', )

    def __init__(self, *, exprs: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.exprs = exprs

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        exprs_expr = []  # type: List[ast_fqn.Base]
        for exprs_item in self.exprs:
            exprs_expr.append(exprs_item.anchors_expr(ctx, anchors))
        return ast_fqn.ExprList(items=exprs_expr)


class ExceptClauseNode(Base):
    """Corresponds to `except_clause`."""

    __slots__ = ('expr', 'as_item')

    def __init__(self, *, expr: Base, as_item: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.as_item = as_item

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.expr.anchors(ctx, anchors)
        self.as_item.anchors(ctx, anchors)
        # TODO: output Binds info
        return None


class FileInput(Base):
    """Corresponds to `file_input`."""

    __slots__ = ('stmts', 'scope_bindings')

    def __init__(self, *, stmts: Sequence[Base],
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.stmts = stmts
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        file_ctx = ctx._replace(
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, ctx.fqn_dot + name)
                                        for name in self.scope_bindings)))
        for stmt in self.stmts:
            # TODO: remove the try/except (it's for debugging)
            try:
                stmt.anchors(file_ctx, anchors)
            except AssertionError as exc:
                raise RuntimeError('%r: %r' % (exc, stmt))
        return None


class ForStmt(Base):
    """Corresponds to `for_stmt`."""

    __slots__ = ('exprlist', 'testlist', 'suite', 'else_suite')

    def __init__(self, *, exprlist: Base, testlist: Base, suite: Base,
                 else_suite: Base) -> None:
        # pylint: disable=super-init-not-called
        self.exprlist = exprlist
        self.testlist = testlist
        self.suite = suite
        self.else_suite = else_suite

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.exprlist.anchors(ctx, anchors)  # Adds to ctx
        self.testlist.anchors(ctx, anchors)
        self.suite.anchors(ctx, anchors)
        self.else_suite.anchors(ctx, anchors)
        return None


class FuncDefStmt(Base):
    """Corresponds to `funcdef` / `async_funcdef` or lambdadef.

    If it's a lambda, the `name` points to the `lambda` keyword and
    this can appear in an "expression" context, hence this is a
    subclass of Base and not of Base.
    """

    __slots__ = ('name', 'parameters', 'return_type', 'suite',
                 'scope_bindings')

    def __init__(self, *, name: 'NameNode', parameters: Sequence[Base],
                 return_type: Base, suite: Base,
                 scope_bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.parameters = parameters
        self.return_type = return_type
        self.suite = suite
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
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
        anchors.append(kythe.FuncDefAnchor(astn=self.name.astn, fqn=func_fqn))
        return_type_expr = self.return_type.anchors_expr(func_ctx, anchors)
        for parameter in self.parameters:
            parameter.anchors(func_ctx, anchors)
        self.suite.anchors(func_ctx, anchors)
        return ast_fqn.FuncDef(fqn=func_fqn, return_type=return_type_expr)


class GlobalStmt(Base):
    """Corresponds to `global_stmt`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[NameNode], names)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for names_item in self.names:
            names_item.anchors(ctx, anchors)
        return None


class IfStmt(ListStmtBase):
    """Corresponds to `if_stmt`."""


class ImportAsNamesNode(Base):
    """Corresponds to `import_as_names`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(AsNameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[AsNameNode], names)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for names_item in self.names:
            names_item.anchors(ctx, anchors)
        return None


class ImportFromStmt(Base):
    """Corresponds to `import_name`."""

    __slots__ = ('from_name', 'import_part')

    def __init__(self, *, from_name: Sequence[Base],
                 import_part: Base) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(DottedNameNode,
                                           from_name)  # TODO: remove
        self.from_name = typing.cast(Sequence[DottedNameNode], from_name)
        assert isinstance(import_part,
                          (ImportAsNamesNode, StarNode))  # TODO: remove
        # TODO: self.import_part = typing.cast(Union[ImportAsNamesNode, StarNode], import_part)
        self.import_part = import_part

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for from_name_item in self.from_name:
            from_name_item.anchors(ctx, anchors)
        self.import_part.anchors(ctx, anchors)
        return None


class ImportNameNode(Base):
    """Corresponds to `import_name`."""

    __slots__ = ('dotted_as_names', )

    def __init__(self, *, dotted_as_names: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_as_names = xcast(DottedAsNamesNode, dotted_as_names)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.dotted_as_names.anchors(ctx, anchors)
        return None


class ListMakerNode(ListExprBase):
    """Corresponds to `listmaker` without `comp_for`."""

    result_type = ast_fqn.ListMaker


class NameNode(Base):
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

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
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
                anchors.append(kythe.BindingAnchor(astn=self.astn, fqn=fqn))
            else:
                anchors.append(kythe.RefAnchor(astn=self.astn, fqn=fqn))
        return ast_fqn.Name(fqn=fqn)


class NonLocalStmt(Base):
    """Corresponds to "nonlocal" variant of `global_stmt`."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameNode, names)  # TODO: remove
        self.names = typing.cast(Sequence[NameNode], names)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for names_item in self.names:
            names_item.anchors(ctx, anchors)
        return None


class NumberNode(Base):
    """Corresponds to a NUMBER node.

    Attributes:
    astn: The AST node of the number
    """

    __slots__ = ('astn', )

    def __init__(self, *, astn: pytree.Leaf) -> None:
        # pylint: disable=super-init-not-called
        self.astn = astn

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return ast_fqn.Number(value=self.astn.value)


class OmittedNode(Base):
    """An item that is omitted (e.g., bases for a class)."""

    __slots__ = ()

    def scope_ctx(self, ctx: FqnCtx) -> FqnCtx:  # pylint: disable=no-self-use
        # For nodes that can be Union[CompForNode, OmittedNode]
        return ctx

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return ast_fqn.OMITTED


# Singleton OmittedNode, to avoid creating many of them.
OMITTED_NODE = OmittedNode()


class OpNode(Base):
    """Corresponds to various expression nodes (unary, binary, comparison)."""

    __slots__ = ('op_astns', 'args')

    def __init__(self, *, op_astns: Sequence[pytree.Base],
                 args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(pytree.Leaf,
                                           op_astns)  # TODO: remove
        self.op_astns = typing.cast(Sequence[pytree.Leaf], op_astns)
        self.args = args

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        args_expr = []  # type: List[ast_fqn.Base]
        for args_item in self.args:
            args_expr.append(args_item.anchors_expr(ctx, anchors))
        return ast_fqn.Op(items=args_expr)


class PassStmt(LeafStmtBase):
    """Corresponds to `pass_stmt`."""


class PrintStmt(ListStmtBase):
    """Corresponds to `print_stmt`."""


class RaiseStmt(ListStmtBase):
    """Corresponds to `raise_stmt`."""


class StarNode(Base):
    """Corresponds to `'*' expr`."""

    __slots__ = ()

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return None


class Stmts(ListStmtBase):
    """Corresponds to `simple_stmt`, `suite`."""


class StringNode(Base):
    """Corresponds to a STRING node.

    Attributes:
        astns: The AST nodes of the string
    """

    __slots__ = ('astns', )

    def __init__(self, *, astns: Sequence[pytree.Leaf]) -> None:
        # pylint: disable=super-init-not-called
        self.astns = astns

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        return ast_fqn.String(value=''.join(astn.value for astn in self.astns))


class SubscriptNode(Base):
    """Corresponds to `subscript`."""

    __slots__ = ('expr1', 'expr2', 'expr3')

    def __init__(self, *, expr1: Base, expr2: Base, expr3: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        items_expr = [
            self.expr1.anchors_expr(ctx, anchors),
            self.expr2.anchors_expr(ctx, anchors),
            self.expr3.anchors_expr(ctx, anchors)
        ]
        return ast_fqn.Subscript(items=items_expr)


class SubscriptListNode(Base):
    """Corresponds to `subscript_list`."""

    __slots__ = ('subscripts', )

    def __init__(self, *, subscripts: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(SubscriptNode,
                                           subscripts)  # TODO: remove
        self.subscripts = typing.cast(Sequence[SubscriptNode], subscripts)

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        subscripts_expr = []  # type: List[ast_fqn.Base]
        for subscripts_item in self.subscripts:
            subscripts_expr.append(subscripts_item.anchors_expr(ctx, anchors))
        return ast_fqn.SubscriptList(items=subscripts_expr)


class TestListNode(ListExprBase):
    """Corresponds to `testlist`, `testlist1`, `testlist_gexp`
    `testlist_star_expr` without `comp_for`."""

    result_type = ast_fqn.TestList


class TnameNode(Base):
    """Corresponds to `tname`."""

    __slots__ = ('name', 'type_expr')

    def __init__(self, *, name: Base, type_expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(NameNode, name)
        self.type_expr = type_expr

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.name.anchors(ctx, anchors)
        self.type_expr.anchors(ctx, anchors)
        return None


class TfpListNode(ListExprBase):
    """Corresponds to `tfplist`."""

    result_type = ast_fqn.TfpList


class TryStmt(ListStmtBase):
    """Corresponds to `try_stmt`."""


class TypedArgNode(Base):
    """Corresponds to `typed_arg`."""

    __slots__ = ('name', 'expr')

    def __init__(self, *, name: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(TnameNode, name)
        self.expr = expr

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.name.anchors(ctx, anchors)
        self.expr.anchors(ctx, anchors)
        return None


class TypedArgsListNode(Base):
    """Corresponds to `typedargslist`.

    This is only used when processing a funcdef; the args are given
    directly to FuncDefStmt, which is why anchors() isn't
    defined for TypedArgsListNode.
    """

    __slots__ = ('args', )

    def __init__(self, *, args: Sequence[TypedArgNode]) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        # Not used anywhere
        raise NotImplementedError(self)


class WhileStmt(ListStmtBase):
    """Corresponds to `while_stmt`."""

    __slots__ = ('test', 'suite', 'else_suite')

    def __init__(self, *, test: Base, suite: Base, else_suite: Base) -> None:
        # pylint: disable=super-init-not-called
        self.test = test
        self.suite = suite
        self.else_suite = else_suite

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.test.anchors(ctx, anchors)
        self.suite.anchors(ctx, anchors)
        self.else_suite.anchors(ctx, anchors)
        return None


class WithItemNode(Base):
    """Corresponds to `with_item`."""

    __slots__ = ('item', 'as_item')

    def __init__(self, *, item: Base, as_item: Base) -> None:
        # pylint: disable=super-init-not-called
        self.item = xcast(AtomTrailerNode, item)
        self.as_item = as_item

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        self.item.anchors(ctx, anchors)
        self.as_item.anchors(ctx, anchors)
        return None


class WithStmt(Base):
    """Corresponds to `with_stmt`."""

    __slots__ = ('items', 'suite')

    def __init__(self, *, items: Sequence[Base], suite: Base) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(WithItemNode, items)  # TODO: remove
        self.items = typing.cast(Sequence[WithItemNode], items)
        self.suite = suite

    def anchors(self, ctx: FqnCtx,
                anchors: List[kythe.Anchor]) -> Optional[ast_fqn.Base]:
        for items_item in self.items:
            items_item.anchors(ctx, anchors)
        self.suite.anchors(ctx, anchors)
        return None


class YieldNode(ListExprBase):
    """Corresponds to `yield_expr`."""

    result_type = ast_fqn.Yield
