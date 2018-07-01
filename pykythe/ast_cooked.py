"""Representation of nodes, for further processing.

ast_raw.cvt traverses an AST (in the lib2to3.pytree format) and puts
it into a more easy to process form. While traversing, it also marks
binding and non-biding uses of all the namnes (including handling of
names that were marked "global" or "nonlocal").

Each node is a subclass of Base.

Here is an example of how this data is used to generate
cross-reference information. Suppose we have in m.py:

    #- @C defines/binding C=vname("m.C", _, _, "", python)
    #- C.node/kind record
    #- C.subkind class
    class C:
      #- @self defines/binding Self=vname("m.C.self", _, _, "", python)
      #- // TODO: add links for self being of type C
      def __init__(self):
        #- @self ref Self
        #- @f1 defines/binding C_F1=vname("m.C.f1", _, _, "", python)
        #- C_F1 childof C
        self.f1 = 0

    #- @C ref C
    #- @c defines/binding VarC=vname("m.c", _, _, "", python)
    c = C()
    #- @c ref VarC
    #- @f1 ref C_F1
    print(c.f1)

To generate these facts, we need to record all the assignment
information in the program and then compute types. In this example, we
need to mark that `self` in C.__init__ is of type `C` and so is `c` in
the main program (in both cases, the `f1` will refer to the same Kythe
semantic node, identified by `C.f1`). For the latter, we capture that
`c` is defined by the expression `call(id('m.C'))` -- when we
"evaluate" this later, we see that `m.C` is defined by `class(C)`,
and we can therefore deduce that `c` is also of type `class(C)`.
"""

from __future__ import annotations
import collections
import dataclasses
from dataclasses import dataclass
import functools
import logging  # pylint: disable=unused-import
from typing import (  # pylint: disable=unused-import
    Any, Mapping, MutableMapping, Iterable, List, Optional, Sequence, Text)
import typing
from mypy_extensions import Arg  # pylint: disable=unused-import

from . import ast, pod, typing_debug
from .typing_debug import cast as xcast

# pylint: disable=too-few-public-methods
# pylint: disable-msg=too-many-arguments
# pylint: disable=too-many-lines


@dataclass(frozen=True)
class FqnCtx(pod.PlainOldData):
    """Context for computing FQNs (fully qualified names).

    Attributes:
      fqn_dot: The Fully Qualifed Name of this scope
          (module/function/class), followed by a '.'
      bindings: mappings of names to FQNs at this scope
      class_fqn: either None if not within a class or the
                 FQN of the enclosing class.
      class_astn: class name's ASTN or None (if not within a class).
      python_version: 2 or 3
    """

    fqn_dot: Text
    bindings: typing.ChainMap[Text, Text]  # pylint: disable=no-member
    class_fqn: Optional[Text]
    class_astn: Optional[ast.Astn]
    python_version: int

    __slots__ = [
        'fqn_dot', 'bindings', 'class_fqn', 'class_astn', 'python_version']


class Base(pod.PlainOldDataExtended):
    """Base class for data from AST nodes.

    These mostly correspond to nodes in lib2to3.pytree.{Node,Leaf},
    with some modifications. Some of them are generated from the
    add_fqns() method.

    This class should not be called directly.
    """

    # TODO: https://github.com/python/mypy/issues/4547

    __slots__ = []

    def __init__(self, **kwargs: Any) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not Base, "Must not instantiate ast_cooked.Node"
        for attr, value in kwargs.items():
            setattr(self, attr, value)  # pragma: no cover

    def add_fqns(self, ctx: FqnCtx) -> Base:
        """Generate a new tree with FQNs filled in.

        This defines the generic form, using self.__slots__.  In a few
        cases (e.g., those that have an attr that is Sequence[Base]),
        this method is overriden.

        A Kythe "anchor" is a pointer to a piece of source code
        (typically, a "name" of some kind in Python) to which semantic
        information is attached.

        In most cases, this method simply recursively calls `add_fqns on its
        contents and returns a new node with the results.

        A few nodes are special, such as `FuncDefStmt` and
        `NameRefNode`. These generate fully qualified names (FQNs)
        that are corpus-wide unique and are used for Kythe `ref` and
        `defines/binding`. A FQN is a list of names separated by '.'
        that gives the name hierarchy. There are some examples in the
        tests. In some cases, a different node is returned with the
        extra information (e.g., `NameRefFqn`).

        This code assumes that all subnodes of type NameNode have had
        the `binds` attribute set properly (ast_raw.cvt does this when
        creating each Node).

        Arguments:
          ctx: The context for generating the FQN information (mainly
               the FQN of the enclosing scope).
        Returns:
          A new node that transforms names to FQNs. Usually it is the
          same type as the original node but in a few cases (e.g., NameNode),
          something different is returned.
        """
        attr_values = {
            attr: self.attr_add_fqns(attr, ctx)
            for attr in self.__slots__}
        # TODO: https://github.com/python/mypy/issues/4602
        return self.__class__(**attr_values)  # type: ignore

    def attr_add_fqns(self, attr: Text, ctx: FqnCtx) -> Base:
        # TODO: inline this when fully debugged
        try:
            return xcast(Base, getattr(self, attr).add_fqns(ctx))
        except Exception as exc:
            raise RuntimeError(
                '%r node=%r:%r' % (exc, attr, getattr(self, attr))) from exc

    def atom_trailer_node(self, atom: Base) -> Base:
        """For processing atom, trailer part of power.

        This is implemented only for nodes that are the result of the
        grammar rule `power: [AWAIT] atom trailer* ['**' factor]`.
        """
        # The following code stops pylint abstract-method from triggering
        # in the classes that don't define it.
        if False:  # pylint: disable=using-constant-test
            raise NotImplementedError(self)  # pragma: no cover
        return Base(atom=atom)  # pragma: no cover


@dataclass(frozen=True)
class ListBase(Base):
    """A convenience class for AST nodes (expr) that contain a single list."""

    items: Sequence[Base]

    __slots__ = ['items']

    def __post_init__(self) -> None:
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not ListBase, (
            "Must not instantiate ast_cooked.ListBase")

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self.__class__(  # type: ignore  # TODO: https://github.com/python/mypy/issues/4602
            items=[_add_fqns_wrap(item, ctx) for item in self.items])


@dataclass(frozen=True)
class EmptyBase(Base):
    """A convenience class for AST nodes (expr) that contain nothing."""

    __slots__ = []

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


def _add_fqns_wrap(item: Base, ctx: FqnCtx) -> Base:
    # TODO: inline this when fully debugged
    try:
        return xcast(Base, item.add_fqns(ctx))
    except Exception as exc:
        raise RuntimeError('%r node=%r' % (exc, item)) from exc


@dataclass(frozen=True)
class AnnAssignNode(Base):
    """Corresponds to `annassign` (expr can be OmittedNode).

    This is only used when processing an annassign statement;
    ast_raw.cvt_expr_stmt directly uses the contents of this node.
    """

    # TODO: also use for # type: ... comment
    # TODO: test case (see ast_raw.cvt_annassign)

    left_annotation: Base
    expr: Base

    __slots__ = ['left_annotation', 'expr']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class AnnAssignStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr annassign`.

    `expr` can be OmittedNode

    For `add_fqns`, `left` is processed after `left_annotation`,`expr`
    -- this is why the __slots__ and args to __init_ have `left` last.
    """

    left_annotation: Base
    expr: Base
    left: Base

    __slots__ = ['left_annotation', 'expr', 'left']


@dataclass(frozen=True)
class ArgListNode(Base):
    """Corresponds to `arglist`.

    This is only usesd when processing a `decorator`, `classdef`, or
    `trailers`, and is incorporated directly into the appropriate node.
    """

    args: Sequence[Base]

    __slots__ = ['args']

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomCallNode(atom=atom, args=self.args)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class ArgumentNode(Base):
    """Corresponds to `argument: test '=' test`.

    The grammar has additional cases for `argument`, but
    ast_raw.cvt_argument removes them by returning the child node.
    """

    name: ast.Astn
    arg: Base

    __slots__ = ['name', 'arg']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return ArgumentNode(name=self.name, arg=_add_fqns_wrap(self.arg, ctx))


@dataclass(frozen=True)
class AsNameNode(Base):
    """Corresponds to `import_as_name`."""

    name: Base
    as_name: Base

    __slots__ = ['name', 'as_name']


@dataclass(frozen=True)
class AssignExprStmt(Base):
    """Corresponds to a single assignment from AssignMultipleExprStmt (q.v.)."""

    left: Base
    expr: Base

    __slots__ = ['left', 'expr']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # disallow reprocessing, which could change things if a `left` contains
        # something that's in the `expr`.
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class AssignMultipleExprStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr ('=' (yield_expr|testlist_star_expr))*`.

    left_list is a list of items found on the left-hand-side of `=`s
    (it can be empty).

    This node is not returned from add_fqns(); instead either ExprStmt
    (if left_list is empty) or multiple AssignExprStmt's, each with
    the same `expr`.

    For `add_fqns`, `left_list` is processed after `expr` -- this is why
    the __slots__ and args to __init_ have `left_list` last.

    # TODO: if multiple "="s (`left_list`), then create a temporary to
            contain the expr and use that.
    """

    expr: Base
    left_list: Sequence[Base]

    __slots__ = ['expr', 'left_list']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        expr = _add_fqns_wrap(self.expr, ctx)
        # add_fqns(ctx) can modify the bindings in ctx, so need to be
        # careful to do things in the right order.
        left_add_fqns = list(
            reversed([
                _add_fqns_wrap(item, ctx)
                for item in reversed(self.left_list)]))
        if len(left_add_fqns) == 1:
            return AssignExprStmt(left=left_add_fqns[0], expr=expr)
        if len(left_add_fqns) > 1:
            return make_stmts(
                AssignExprStmt(left=left, expr=expr) for left in left_add_fqns)
        return ExprStmt(expr=expr)


class AssertStmt(ListBase):
    """Corresponds to `assert_stmt`."""


def atom_trailer_node(atom: Base, trailers: Sequence[Base]) -> Base:
    """Create the appropriate AtomXXX nodes."""
    return functools.reduce(
        lambda atom, trailer: trailer.atom_trailer_node(atom), trailers, atom)


@dataclass(frozen=True)
class AtomCallNode(Base):
    """Corresponds to `atom '(' [arglist] ')'`."""

    atom: Base
    args: Sequence[Base]

    __slots__ = ['atom', 'args']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AtomCallNode(
            atom=_add_fqns_wrap(self.atom, ctx),
            args=[_add_fqns_wrap(arg, ctx) for arg in self.args])


@dataclass(frozen=True)
class AtomDotNode(Base):
    """Corresponds to `atom '.' NAME`."""

    atom: Base
    attr_name: ast.Astn
    binds: bool

    # TODO: is `binds` needed or can it be inferred?
    __slots__ = ['atom', 'attr_name', 'binds']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AtomDotNode(
            atom=_add_fqns_wrap(self.atom, ctx),
            attr_name=self.attr_name,
            binds=self.binds)


@dataclass(frozen=True)
class AtomSubscriptNode(Base):
    """Corresponds to `atom '[' [subscriptist] ']'`."""

    atom: Base
    subscripts: Sequence[Base]

    __slots__ = ['atom', 'subscripts']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AtomSubscriptNode(
            atom=_add_fqns_wrap(self.atom, ctx),
            subscripts=[
                _add_fqns_wrap(subscript, ctx)
                for subscript in self.subscripts])


@dataclass(frozen=True)
class AugAssignNode(Base):
    """Corresponds to `augassign`."""

    op: ast.Astn

    __slots__ = ['op']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class AugAssignStmt(Base):
    """Corresponds to expr_stmt: augassign (yield_expr|testlist).

    For add_fqns, `left` is processed after ``expr` -- this is why the
    __slots__ and args to __init_ have `left_list` last.
    """

    augassign: ast.Astn
    expr: Base
    left: Base

    __slots__ = ['augassign', 'expr', 'left']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AugAssignStmt(
            augassign=self.augassign,
            expr=_add_fqns_wrap(self.expr, ctx),
            left=_add_fqns_wrap(self.left, ctx))


class BreakStmt(EmptyBase):
    """Corresponds to `break_stmt`."""


@dataclass(frozen=True)
class Class(Base):
    """Created by ClassDefStmt.add_fqns()."""

    fqn: Text
    name: ast.Astn
    bases: Sequence[Base]

    __slots__ = ['fqn', 'name', 'bases']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class ClassDefStmt(Base):
    """Corresponds to `classdef`."""

    name: NameBindsNode
    bases: Sequence[Base]
    suite: Base
    scope_bindings: Mapping[Text, None]

    __slots__ = ['name', 'bases', 'suite', 'scope_bindings']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Similar to FuncDefStmt.add_fqns
        class_fqn = ctx.fqn_dot + self.name.name.value
        class_fqn_dot = class_fqn + '.'
        name_add_fqns = xcast(NameBindsFqn, _add_fqns_wrap(
            self.name, ctx))  # already in bindings
        class_ctx = dataclasses.replace(
            ctx,
            fqn_dot=class_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, class_fqn_dot + name)
                                        for name in self.scope_bindings)),
            class_fqn=class_fqn,
            class_astn=self.name.name)
        class_add_fqns = Class(
            fqn=class_fqn,
            name=name_add_fqns.name,
            bases=[_add_fqns_wrap(base, ctx) for base in self.bases])
        return make_stmts([
            class_add_fqns,
            _add_fqns_wrap(self.suite, class_ctx)])


@dataclass(frozen=True)
class CompForNode(Base):
    """Corresponds to `comp_for`.

    Note that for_exprlist isn't necessarily a list (e.g., to distinguish
    between `for foo= ...` and for foo,=...`)
    """

    for_astn: ast.Astn
    for_exprlist: Base
    in_testlist: Base
    comp_iter: Base
    scope_bindings: Mapping[Text, None]

    __slots__ = [
        'for_astn', 'for_exprlist', 'in_testlist', 'comp_iter',
        'scope_bindings']

    def scope_ctx(self, ctx: FqnCtx) -> FqnCtx:
        """New FqnCtx for the scope of the comp_for (updates ctx for Python2).

        Used by DictGenListSetMakerCompForNode.
        """
        if ctx.python_version == 2:  # pragma: no cover
            # The bindings "leak" in Python2
            ctx.bindings.update((name, ctx.fqn_dot + name)
                                for name in self.scope_bindings)
            return ctx
        for_fqn_dot = '{}<comp_for>[{:d},{:d}].'.format(
            ctx.fqn_dot, self.for_astn.start, self.for_astn.end)
        assert for_fqn_dot != 'pykythe.test_data.bindings.testListFor.<local>.<comp_for>[2567,2570].', [for_fqn_dot, ctx]  # DO NOT SUBMIT
        return xcast(FqnCtx, dataclasses.replace(
            ctx,
            fqn_dot=for_fqn_dot,
            bindings=ctx.bindings.new_child(collections.OrderedDict())))

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Assume that the caller has created a new child in the
        # bindings, if needed.  This is done at the outermost level of
        # a comp_for (for Python 3), but not for any of the inner
        # comp_for's.
        # This handles the following:
        #    x for x in [1,x]  # `x` in `[1,x]` is outer scope
        #    (x, y) for x in [1,2] for y in range(x)  # `x` in `range(x)` is from `for x`
        # [(x, y) for x in [1,2,x] for y in range(x)]  # error: y undefined
        in_testlist_add_fqns = _add_fqns_wrap(self.in_testlist, ctx)
        ctx.bindings.update((name, ctx.fqn_dot + name)
                            for name in self.scope_bindings)
        for_exprlist_add_fqns = _add_fqns_wrap(self.for_exprlist, ctx)
        comp_iter_add_fqns = _add_fqns_wrap(self.comp_iter, ctx)
        return CompFor(
            for_astn=self.for_astn,
            for_exprlist=for_exprlist_add_fqns,
            in_testlist=in_testlist_add_fqns,
            comp_iter=comp_iter_add_fqns)


@dataclass(frozen=True)
class CompFor(Base):
    """Created by CompForNode."""

    for_astn: ast.Astn
    for_exprlist: Base
    in_testlist: Base
    comp_iter: Base

    __slots__ = ['for_astn', 'for_exprlist', 'in_testlist', 'comp_iter']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class CompIfCompIterNode(Base):
    """Corresponds to `comp_if` with `comp_iter`."""

    value_expr: Base
    comp_iter: Base

    __slots__ = ['value_expr', 'comp_iter']


class ContinueStmt(EmptyBase):
    """Corresponds to `continue_stmt`."""


class DecoratorsNode(ListBase):
    """Corresponds to `decorators`."""


class DecoratedStmt(ListBase):
    """Corresponds to `decorated`."""


class DecoratorDottedNameNode(ListBase):
    """Corresponds to `dotted_name` in `decorator` (see also DottedNameNode)."""

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[NameRawNode], items)
        typing_debug.assert_all_isinstance(NameRawNode, self.items)


@dataclass(frozen=True)
class DecoratorNode(Base):
    """Corresponds to `decorator`.

    The grammar specifies the same `dotted_name` as `import_from` and
    `dotted_as_name` use, but we depend on this having been changed
    from DottedNameNode to DecoratorDottedNameNode.
    """

    name: DecoratorDottedNameNode
    args: Sequence[Base]

    __slots__ = ['name', 'args']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AtomCallNode(
            atom=_add_fqns_wrap(self.name, ctx),
            args=[_add_fqns_wrap(arg, ctx) for arg in self.args])


class DelStmt(ListBase):
    """Corresponds to `del_stmt`."""


class DictKeyValue(ListBase):
    """Corresponds to `test ':' test` in DictGenListSetMakerCompForNode."""


class DictSetMakerNode(ListBase):
    """Corresponds to `dictsetmaker` without `comp_for`."""


@dataclass(frozen=True)
class DictGenListSetMakerCompFor(Base):
    """Created by DictGenListSetMakerCompForNode.add_fqns()."""

    value_expr: Base
    comp_for: CompFor

    __slots__ = ['value_expr', 'comp_for']


    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class DictGenListSetMakerCompForNode(Base):
    """Corresponds to {`dict_set_maker', `listmaker`, testlist_gexp`} with
    `comp_for`. For our purposes, it's not important to know whether
    this is a list, set, or dict comprehension
    """

    value_expr: Base
    comp_for: CompForNode

    __slots__ = ['value_expr', 'comp_for']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        comp_for_ctx = self.comp_for.scope_ctx(ctx)
        comp_for = xcast(CompFor, self.comp_for.add_fqns(comp_for_ctx))
        value_expr = self.value_expr.add_fqns(comp_for_ctx)
        return DictGenListSetMakerCompFor(
            value_expr=value_expr, comp_for=comp_for)


@dataclass(frozen=True)
class DotNameTrailerNode(Base):
    """Corresponds to '.' NAME in trailer.

    This is only used when processing a trailer and is incorporated
    directly into AtomDotNode.
    """

    # TODO: Remove binds and instead: DotNameTrailerNode{Binds,Ref}
    #       similar to Name{Binds,Ref}Fqn

    name: NameRawNode
    binds: bool

    __slots__ = ['name', 'binds']

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomDotNode(
            atom=atom, attr_name=self.name.name, binds=self.binds)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class DottedNameNode(ListBase):
    """Corresponds to `dotted_name`."""

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[NameRawNode], items)
        typing_debug.assert_all_isinstance(NameRawNode, self.items)


class EllipsisNode(EmptyBase):
    """Corresponds to `...`."""


class ExecStmt(ListBase):
    """Corresponds to `exec_stmt`."""


class ExprListNode(ListBase):
    """Corresponds to `exprlist`, `testlist`, `testlist1`, `testlist_gexp`
    `testlist_star_expr` without `comp_for`.

    DelStmt doesn't use ExprListNode (despite the grammar); it
    directly stores the items (there's no semantic difference if the
    list ends with `,`).
    """


@dataclass(frozen=True)
class ExprStmt(Base):
    """Corresponds an expr-only from AssignMultipleExprStmt (q.v.)."""

    expr: Base

    __slots__ = ['expr']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # disallow reprocessing, even though probably benign (see AssignExprStmt).
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class ExceptClauseNode(Base):
    """Corresponds to `except_clause`."""

    expr: Base
    as_item: Base

    __slots__ = ['expr', 'as_item']


@dataclass(frozen=True)
class FileInput(Base):
    """Corresponds to `file_input`."""

    path: Text
    stmts: Sequence[Base]
    scope_bindings: Mapping[Text, None]

    __slots__ = ['path', 'stmts', 'scope_bindings']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        file_ctx = dataclasses.replace(
            ctx,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, ctx.fqn_dot + name)
                                        for name in self.scope_bindings)))
        stmts = [_add_fqns_wrap(stmt, file_ctx) for stmt in self.stmts]
        return FileInput(
            path=self.path, stmts=stmts, scope_bindings=self.scope_bindings)


@dataclass(frozen=True)
class ForStmt(Base):
    """Corresponds to `for_stmt`.

    Note that for_exprlist isn't necessarily a list (e.g., to distinguish
    between `for foo= ...` and for foo,=...`)
    """

    for_exprlist: Base
    in_testlist: Base
    suite: Base
    else_suite: Base

    __slots__ = ['for_exprlist', 'in_testlist', 'suite', 'else_suite']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Order is important: in_testlist is in outer bindings context,
        # for_exprlist adds to bindings, suite and else_suite use the
        # additional bindings (and also the bindings "leak" outside
        # the for-loop).
        in_testlist_add_fqns = _add_fqns_wrap(self.in_testlist, ctx)
        # for_exprlist adds to bindings
        for_exprlist_add_fqns = _add_fqns_wrap(self.for_exprlist, ctx)
        return ForStmt(
            for_exprlist=for_exprlist_add_fqns,
            in_testlist=in_testlist_add_fqns,
            suite=_add_fqns_wrap(self.suite, ctx),
            else_suite=_add_fqns_wrap(self.else_suite, ctx))


@dataclass(frozen=True)
class Func(Base):
    """Created by FuncDefStmt.add_fqns()."""

    fqn: Text
    name: ast.Astn
    parameters: Sequence[Base]
    return_type: Base

    __slots__ = ['fqn', 'name', 'parameters', 'return_type']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class FuncDefStmt(Base):
    """Corresponds to `funcdef` / `async_funcdef` or lambdadef.

    If it's a lambda, the `name` points to the `lambda` keyword and
    this can appear in an "expression" context, hence this is a
    subclass of Base and not of Base.
    """

    name: NameBindsNode
    parameters: Sequence[Base]
    return_type: Base
    suite: Base
    scope_bindings: Mapping[Text, None]

    __slots__ = [
        'name', 'parameters', 'return_type', 'suite', 'scope_bindings']

    # attr_add_fqns not needed because add_fqns() method is overriden.

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Similar to ClassDefStmt.add_fqns
        # '.<local>.' is needed to distinguish `x` in following:
        #    def foo(x): pass
        #    foo.x = 'a string'
        if self.name.name.value == 'lambda':
            # Make a unique name for the lambda
            func_fqn = '{}<lambda>[{:d},{:d}]'.format(
                ctx.fqn_dot, self.name.name.start, self.name.name.end)
        else:
            func_fqn = '{}{}'.format(ctx.fqn_dot, self.name.name.value)
        func_fqn_dot = func_fqn + '.<local>.'
        # self.name is already in bindings
        name_add_fqns = xcast(NameBindsFqn, _add_fqns_wrap(self.name, ctx))
        func_ctx = dataclasses.replace(
            ctx,
            fqn_dot=func_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, func_fqn_dot + name)
                                        for name in self.scope_bindings)),
            class_fqn=None,
            class_astn=None)
        # parameters require special handling because the type+default
        # are evaluated in ctx but the name is evaluated in
        # func_ctx. We can assume that the type+default have already
        # been added to the bindings at ctx (or an outer scope, via
        # the ChainMap). Also, if this is a method, we need to
        # specially handle the type for the first parameter if it
        # doesn't have a type annotation or default value.
        if (ctx.class_fqn and ctx.class_astn and self.parameters and
                isinstance(
                    xcast(TypedArgNode, self.parameters[0]).tname.type_expr,
                    OmittedNode) and isinstance(
                        xcast(TypedArgNode, self.parameters[0]).expr,
                        OmittedNode)):
            param0 = TypedArgNode(
                tname=TnameNode(
                    name=_add_fqns_wrap(
                        xcast(TypedArgNode, self.parameters[0]).tname.name,
                        func_ctx),
                    type_expr=NameRefGenerated(fqn=ctx.class_fqn)),
                expr=OMITTED_NODE)
            parameters = [param0] + [
                xcast(TypedArgNode, _add_fqns_wrap(parameter, func_ctx))
                for parameter in self.parameters[1:]]
        else:
            parameters = [
                xcast(TypedArgNode, _add_fqns_wrap(parameter, func_ctx))
                for parameter in self.parameters]
        func_add_fqns = Func(
            fqn=func_fqn,
            name=name_add_fqns.name,
            parameters=parameters,
            return_type=_add_fqns_wrap(self.return_type, ctx))
        return make_stmts([
            func_add_fqns, _add_fqns_wrap(self.suite, func_ctx)])


class GlobalStmt(ListBase):
    """Corresponds to `global_stmt`."""


class IfStmt(ListBase):
    """Corresponds to `if_stmt`."""


class ImportAsNamesNode(ListBase):
    """Corresponds to `import_as_names`."""


@dataclass(frozen=True)
class ImportDotNode(Base):
    """Corresponds to a DOT in `import_from`."""

    __slots__ = []


@dataclass(frozen=True)
class ImportDottedAsNameFqn(Base):
    """Created by ImportDottedAsNameNode.add_fqns."""

    dotted_name: DottedNameNode
    as_name: NameBindsFqn

    __slots__ = ['dotted_name', 'as_name']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


@dataclass(frozen=True)
class ImportDottedAsNameNode(Base):
    """Corresponds to `dotted_as_name`.

    This is only used by `import_name`, so if the `as` is missing,
    then the first item in the `dotted_name` gets marked as "binds".
    """

    dotted_name: DottedNameNode
    as_name: NameBindsNode

    __slots__ = ['dotted_name', 'as_name']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return ImportDottedAsNameFqn(
            dotted_name=xcast(DottedNameNode,
                _add_fqns_wrap(self.dotted_name, ctx)),
            as_name=xcast(NameBindsFqn,
                _add_fqns_wrap(self.as_name, ctx)))


class ImportDottedAsNamesFqn(ListBase):
    """Corresponds to `dotted_as_names`."""

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[ImportDottedAsNameFqn], items)
        typing_debug.assert_all_isinstance(
            ImportDottedAsNameFqn, self.items)  # TODO: remove

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


class ImportDottedAsNamesNode(ListBase):
    """Created by ImportDottedAsNamesNode.add_fqns."""

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[ImportDottedAsNameNode], items)
        typing_debug.assert_all_isinstance(
            ImportDottedAsNameNode, self.items)  # TODO: remove

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return ImportDottedAsNamesFqn(
            items=[_add_fqns_wrap(item, ctx) for item in self.items])


@dataclass(frozen=True)
class ImportFromStmt(Base):
    """Corresponds to `import_name`."""

    from_name: Sequence[Base]
    import_part: Base

    __slots__ = ['from_name', 'import_part']

    def __post_init__(self) -> None:
        typing_debug.assert_all_isinstance((DottedNameNode, ImportDotNode),
                                           self.from_name)
        # self.from_name = typing.cast(Sequence[DottedNameNode], from_name)
        assert isinstance(
            self.import_part, (ImportAsNamesNode, StarNode))
        # TODO: self.import_part = typing.cast(Union[ImportAsNamesNode, StarNode], import_part)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # ast_raw.cvt_import_from has made sure that the names in
        # import_part are NameBindsNode, so we don't need to do
        # anything special about them.
        return ImportFromStmt(
            from_name=[_add_fqns_wrap(name, ctx) for name in self.from_name],
            import_part=_add_fqns_wrap(self.import_part, ctx))


@dataclass(frozen=True)
class ImportNameFqn(Base):
    """Created by ImportNameNode.add_fqns."""

    dotted_as_names: ImportDottedAsNamesFqn

    __slots__ = ['dotted_as_names']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


@dataclass(frozen=True)
class ImportNameNode(Base):
    """Corresponds to `import_name`."""

    dotted_as_names: Base

    __slots__ = ['dotted_as_names']

    def __post_init__(self) -> None:
        # self.dotted_as_names = xcast(ImportDottedAsNamesNode, dotted_as_names)
        assert isinstance(self.dotted_as_names, ImportDottedAsNamesNode)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return ImportNameFqn(
            dotted_as_names=xcast(ImportDottedAsNamesFqn,
                                  _add_fqns_wrap(self.dotted_as_names, ctx)))


class ListMakerNode(ListBase):
    """Corresponds to `listmaker` without `comp_for`."""


@dataclass(frozen=True)
class NameBindsFqn(Base):
    """Created by NameBindsNode.add_fqns."""

    name: ast.Astn
    fqn: Text

    __slots__ = ['name', 'fqn']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class NameBindsNode(Base):
    """Corresponds to a NAME node, in binding context.

    Attributes:
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    name: ast.Astn

    __slots__ = ['name']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        name = self.name.value
        # There are some obscure cases where fqn doesn't get filled
        # in, typically due to the grammar accepting an illegal Python
        # program (e.g., the grammar allows test=test for an arg, but
        # it should be NAME=test)
        if name in ctx.bindings:
            fqn = ctx.bindings[name] or ''
        else:
            fqn = ctx.fqn_dot + name
            ctx.bindings[name] = fqn
        return NameBindsFqn(name=self.name, fqn=fqn)


@dataclass(frozen=True)
class NameRawNode(Base):
    """Corresponds to a NAME node that doesn't get a FQN.

    This is just a wrapper for ast.Astn, so that ast_raw type-checking
    works (an alternative would be to convolute things so that
    ast.Astn is a subclass of ast_cooked.Base).

    Attributes:
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    name: ast.Astn

    __slots__ = ['name']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class NameRefNode(Base):
    """Corresponds to a NAME node, in ref contet.

    Attributes:
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    name: ast.Astn

    __slots__ = ['name']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        name = self.name.value
        # There are some obscure cases where fqn doesn't get filled
        # in, typically due to the grammar accepting an illegal Python
        # program (e.g., the grammar allows test=test for an arg, but
        # it should be NAME=test)
        if name in ctx.bindings:
            fqn = ctx.bindings[name] or ''
        else:
            fqn = ctx.fqn_dot + name
            ctx.bindings[name] = fqn
        return NameRefFqn(name=self.name, fqn=fqn)


@dataclass(frozen=True)
class NameRefFqn(Base):
    """Created by NameRefNode.add_fqns."""

    name: ast.Astn
    fqn: Text

    __slots__ = ['name', 'fqn']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class NameRefGenerated(Base):
    """Like NameRef, but for `self` type nodes.

    This is used to distinguish between nodes that should generate
    add_fqns and those that are generated (e.g., for handling the
    `self` in method definitions and don't need `add_fqns`).
    """

    fqn: Text

    __slots__ = ['fqn']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class NonLocalStmt(ListBase):
    """Corresponds to "nonlocal" variant of `global_stmt`."""


@dataclass(frozen=True)
class NumberNode(Base):
    """Corresponds to a NUMBER node.

    Attributes:
    astn: The AST node of the number
    """

    astn: ast.Astn

    __slots__ = ['astn']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


class OmittedNode(EmptyBase):
    """An item that is omitted (e.g., bases for a class)."""


# Singleton OmittedNode, to avoid creating many of them.
OMITTED_NODE = OmittedNode()


class OpNode(Base):
    """Corresponds to various expression nodes (unary, binary, comparison)."""

    op_astns: Sequence[ast.Astn]
    args: Sequence[Base]

    __slots__ = ['op_astns', 'args']

    def __post_init__(self) -> None:
        typing_debug.assert_all_isinstance(ast.Astn, self.op_astns)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return OpNode(
            op_astns=self.op_astns,
            args=[_add_fqns_wrap(arg, ctx) for arg in self.args])


class PassStmt(EmptyBase):
    """Corresponds to `pass_stmt`."""


class PrintStmt(ListBase):
    """Corresponds to `print_stmt`."""


class RaiseStmt(ListBase):
    """Corresponds to `raise_stmt`."""


class StarNode(EmptyBase):
    """Corresponds to `'*' expr`."""


class Stmts(ListBase):
    """Corresponds to `simple_stmt`, `suite`.

    Should never be created directly, but through the `make_stmts` factory.
    """


def make_stmts(items: Iterable[Base]) -> Stmts:
    """Create Stmts node, flattening any Stmts in items."""
    flattened_items = []  # type: List[Base]
    for item in items:
        if isinstance(item, Stmts):
            flattened_items.extend(item.items)
        else:
            flattened_items.append(item)
    return Stmts(items=flattened_items)


@dataclass(frozen=True)
class StringNode(Base):
    """Corresponds to a STRING node.

    Attributes:
        astns: The AST nodes of the string
    """

    astns: Sequence[ast.Astn]

    __slots__ = ['astns']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class SubscriptNode(Base):
    """Corresponds to `subscript`."""

    expr1: Base
    expr2: Base
    expr3: Base

    __slots__ = ['expr1', 'expr2', 'expr3']


@dataclass(frozen=True)
class SubscriptListNode(Base):
    """Corresponds to `subscript_list`.

    This is only used when processing trailers and is incorporated
    directly into AtomSubscriptNode.
    """

    subscripts: Sequence[Base]

    __slots__ = ['subscripts']

    def __post_init__(self) -> None:
        typing_debug.assert_all_isinstance(SubscriptNode, self.subscripts)
        # self.subscripts = typing.cast(Sequence[SubscriptNode], subscripts)

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomSubscriptNode(atom=atom, subscripts=self.subscripts)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class TestListNode(ListBase):
    """Corresponds to ."""


@dataclass(frozen=True)
class TnameNode(Base):
    """Corresponds to `tname`."""

    name: Base
    type_expr: Base

    __slots__ = ['name', 'type_expr']


class TfpListNode(ListBase):
    """Corresponds to `tfplist`."""

    # TODO: test case (see ast_raw.cvt_tfplist)


class TryStmt(ListBase):
    """Corresponds to `try_stmt`."""


@dataclass(frozen=True)
class TypedArgNode(Base):
    """Corresponds to `typedargslist` `tfpdef ['=' test]` and similar."""

    tname: TnameNode
    expr: Base

    __slots__ = ['tname', 'expr']


@dataclass(frozen=True)
class TypedArgsListNode(Base):
    """Corresponds to `typedargslist`.

    This is only used when processing a funcdef; the args are given
    directly to FuncDefStmt, which is why add_fqns() isn't
    defined for TypedArgsListNode.
    """

    args: Sequence[TypedArgNode]

    __slots__ = ['args']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class WhileStmt(Base):
    """Corresponds to `while_stmt`."""

    test: Base
    suite: Base
    else_suite: Base

    __slots__ = ['test', 'suite', 'else_suite']


@dataclass(frozen=True)
class WithItemNode(Base):
    """Corresponds to `with_item`."""

    item: Base
    as_item: Base

    __slots__ = ['item', 'as_item']


@dataclass(frozen=True)
class WithStmt(Base):
    """Corresponds to `with_stmt`."""

    items: Sequence[Base]
    suite: Base

    __slots__ = ['items', 'suite']

    def __post_init__(self) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(WithItemNode, self.items)
        # self.items = typing.cast(Sequence[WithItemNode], items)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return WithStmt(
            items=[_add_fqns_wrap(item, ctx) for item in self.items],
            suite=_add_fqns_wrap(self.suite, ctx))


class YieldNode(ListBase):
    """Corresponds to `yield_expr`."""

    # TODO: test case (see ast_raw.cvt_yield_expr, ast_raw.cvt_yield_stmt)


# === other facts that are output as JSON


@dataclass(frozen=True)
class Meta(pod.PlainOldDataExtended):
    """Information about the file."""

    kythe_corpus: Text
    kythe_root: Text
    path: Text
    language: Text
    contents_b64: Text
    encoding: Text

    __slots__ = [
        'kythe_corpus', 'kythe_root', 'path', 'language', 'contents_b64',
        'encoding']
