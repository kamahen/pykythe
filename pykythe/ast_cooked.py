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

import collections
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

    __slots__ = [
        'fqn_dot', 'bindings', 'class_fqn', 'class_astn', 'python_version']

    def __init__(
            self,
            *,
            fqn_dot: Text,
            bindings: typing.ChainMap[Text, Text],  # pylint: disable=no-member
            class_fqn: Optional[Text],
            class_astn: Optional[ast.Astn],
            python_version: int) -> None:
        # pylint: disable=super-init-not-called
        self.fqn_dot = fqn_dot
        self.bindings = bindings
        self.class_fqn = class_fqn
        self.class_astn = class_astn
        self.python_version = python_version


class Base(pod.PlainOldDataExtended):
    """Base class for data from AST nodes.

    These mostly correspond to nodes in lib2to3.pytree.{Node,Leaf},
    with some modifications. Some of them are generated from the
    anchors() method.

    This class should not be called directly.
    """

    # TODO: https://github.com/python/mypy/issues/4547

    __slots__ = []  # type: Sequence[str]

    def __init__(self, **kwargs: Any) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not Base, "Must not instantiate ast_cooked.Node"
        for attr, value in kwargs.items():
            setattr(self, attr, value)  # pragma: no cover

    def anchors(self, ctx: FqnCtx) -> 'Base':
        """Generate a new tree with FQNs filled in.

        This defines the generic form, using self.__slots__.  In a few
        cases (e.g., those that have an attr that is Sequence[Base]),
        this method is overriden.

        A Kythe "anchor" is a pointer to a piece of source code
        (typically, a "name" of some kind in Python) to which semantic
        information is attached.

        A few nodes are special, such as FuncDefStmt and
        NameNode. These generate fully qualified names (FQNs) that are
        corpus-wide unique and are used for Kythe `ref` and
        `defines/binding`. A FQN is a list of names separated by '.'
        that gives the name hierarchy. There are some examples in the
        tests.

        This code assumes that all subnodes of type NameNode have had
        the `binds` attribute set properly (ast_raw.cvt does this when
        creating each Node).

        Arguments:
          ctx: The context for generating the FQN information (mainly
               the FQN of the enclosing scope). The `anchors` field
               is appended to.
        Returns:
          A new node that transforms names to FQNs. Usually it is the
          same type as the original node but in a few cases (e.g., NameNode),
          something different is returned.

        """
        attr_values = {
            attr: self.attr_anchors(attr, ctx)
            for attr in self.__slots__}
        # TODO: https://github.com/python/mypy/issues/4602
        return self.__class__(**attr_values)  # type: ignore

    def attr_anchors(self, attr: Text, ctx: FqnCtx) -> 'Base':
        # TODO: inline this when fully debugged
        try:
            return xcast(Base, getattr(self, attr).anchors(ctx))
        except Exception as exc:
            raise RuntimeError(
                '%r node=%r:%r' % (exc, attr, getattr(self, attr))) from exc

    def atom_trailer_node(self, atom: 'Base') -> 'Base':
        """For processing atom, trailer part of power.

        This is implemented only for nodes that are the result of the
        grammar rule `power: [AWAIT] atom trailer* ['**' factor]`.
        """
        # The following code stops pylint abstract-method from triggering
        # in the classes that don't define it.
        if False:  # pylint: disable=using-constant-test
            raise NotImplementedError(self)  # pragma: no cover
        return Base(atom=atom)  # pragma: no cover


class ListBase(Base):
    """A convenience class for AST nodes (expr) that contain a single list."""

    __slots__ = ['items']

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not ListBase, (
            "Must not instantiate ast_cooked.ListBase")
        self.items = items

    def anchors(self, ctx: FqnCtx) -> Base:
        return self.__class__(  # type: ignore  # TODO: https://github.com/python/mypy/issues/4602
            items=[_anchors_wrap(item, ctx) for item in self.items])


class EmptyBase(Base):
    """A convenience class for AST nodes (expr) that contain nothing."""

    def __init__(self) -> None:
        # pylint: disable=super-init-not-called
        pass

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


def _anchors_wrap(item: Base, ctx: FqnCtx) -> Base:
    # TODO: inline this when fully debugged
    try:
        return xcast(Base, item.anchors(ctx))
    except Exception as exc:
        raise RuntimeError('%r node=%r' % (exc, item)) from exc


class AnnAssignNode(Base):
    """Corresponds to `annassign` (expr can be OmittedNode).

    This is only used when processing an annassign statement;
    ast_raw.cvt_expr_stmt directly uses the contents of this node.
    """

    # TODO: also use for # type: ... comment

    __slots__ = ['left_annotation', 'expr']

    def __init__(self, *, left_annotation: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        # TODO: test case (see ast_raw.cvt_annassign)
        self.expr = expr
        self.left_annotation = left_annotation

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class AnnAssignStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr annassign`.

    `expr` can be OmittedNode

    For anchors, `left` is processed after `left_annotation`,`expr` --
    this is why the __slots__ and args to __init_ have `left` last.
    """

    __slots__ = ['left_annotation', 'expr', 'left']

    def __init__(
            self, *, left_annotation: Base, expr: Base, left: Base) -> None:
        # pylint: disable=super-init-not-called
        # TODO: test case (see ast_raw.cvt_expr_stmt)
        self.left_annotation = left_annotation
        self.expr = expr
        self.left = left


class ArgListNode(Base):
    """Corresponds to `arglist`.

    This is only usesd when processing a `decorator`, `classdef`, or
    `trailers`, and is incorporated directly into the appropriate node.
    """

    __slots__ = ['args']

    def __init__(self, *, args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomCallNode(atom=atom, args=self.args)

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class ArgumentNode(Base):
    """Corresponds to `argument: test '=' test`.

    The grammar has additional cases for `argument`, but
    ast_raw.cvt_argument removes them by returning the child node.
    """

    __slots__ = ['name', 'arg']

    def __init__(self, *, name: ast.Astn, arg: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.arg = arg

    def anchors(self, ctx: FqnCtx) -> Base:
        return ArgumentNode(name=self.name, arg=_anchors_wrap(self.arg, ctx))


class AsNameNode(Base):
    """Corresponds to `import_as_name`."""

    __slots__ = ['name', 'as_name']

    def __init__(self, *, name: Base, as_name: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.as_name = as_name


class AssignExprStmt(Base):
    """Corresponds to a single assignment from AssignMultipleExprStmt (q.v.)."""

    __slots__ = ['left', 'expr']

    def __init__(self, *, left: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.left = left

    def anchors(self, ctx: FqnCtx) -> Base:
        # disallow reprocessing, which could change things if a `left` contains
        # something that's in the `expr`.
        raise NotImplementedError(self)  # pragma: no cover


class AssignMultipleExprStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr ('=' (yield_expr|testlist_star_expr))*`.

    left_list is a list of items found on the left-hand-side of `=`s
    (it can be empty).

    This node is not returned from anchors(); instead either ExprStmt
    (if left_list is empty) or multiple AssignExprStmt's, each with
    the same `expr`.

    For anchors, `left_list` is processed after `expr` -- this is why
    the __slots__ and args to __init_ have `left_list` last.

    # TODO: if multiple "="s (`left_list`), then create a temporary to
            contain the expr and use that.
    """

    __slots__ = ['expr', 'left_list']

    def __init__(self, *, expr: Base, left_list: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.left_list = left_list

    def anchors(self, ctx: FqnCtx) -> Base:
        expr = _anchors_wrap(self.expr, ctx)
        # anchors(ctx) can modify the bindings in ctx, so need to be
        # careful to do things in the right order.
        left_anchors = list(
            reversed([
                _anchors_wrap(item, ctx)
                for item in reversed(self.left_list)]))
        if len(left_anchors) == 1:
            return AssignExprStmt(left=left_anchors[0], expr=expr)
        if len(left_anchors) > 1:
            return make_stmts(
                AssignExprStmt(left=left, expr=expr) for left in left_anchors)
        return ExprStmt(expr=expr)


class AssertStmt(ListBase):
    """Corresponds to `assert_stmt`."""


def atom_trailer_node(atom: Base, trailers: Sequence[Base]) -> Base:
    """Create the appropriate AtomXXX nodes."""
    return functools.reduce(
        lambda atom, trailer: trailer.atom_trailer_node(atom), trailers, atom)


class AtomCallNode(Base):
    """Corresponds to `atom '(' [arglist] ')'`."""

    __slots__ = ['atom', 'args']

    def __init__(self, *, atom: Base, args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomCallNode(
            atom=_anchors_wrap(self.atom, ctx),
            args=[_anchors_wrap(arg, ctx) for arg in self.args])


class AtomDotNode(Base):
    """Corresponds to `atom '.' NAME`."""

    # TODO: is `binds` needed or can it be inferred?
    __slots__ = ['atom', 'attr_name', 'binds']

    def __init__(
            self, *, atom: Base, attr_name: ast.Astn, binds: bool) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.attr_name = attr_name
        self.binds = binds

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomDotNode(
            atom=_anchors_wrap(self.atom, ctx),
            attr_name=self.attr_name,
            binds=self.binds)


class AtomSubscriptNode(Base):
    """Corresponds to `atom '[' [subscriptist] ']'`."""

    __slots__ = ['atom', 'subscripts']

    def __init__(self, *, atom: Base, subscripts: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.subscripts = subscripts

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomSubscriptNode(
            atom=_anchors_wrap(self.atom, ctx),
            subscripts=[
                _anchors_wrap(subscript, ctx)
                for subscript in self.subscripts])


class AugAssignNode(Base):
    """Corresponds to `augassign`."""

    __slots__ = ['op']

    def __init__(self, *, op: ast.Astn) -> None:  # pylint: disable=invalid-name
        # pylint: disable=super-init-not-called
        self.op = op  # pylint: disable=invalid-name

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class AugAssignStmt(Base):
    """Corresponds to expr_stmt: augassign (yield_expr|testlist).

    For anchors, `left` is processed after ``expr` -- this is why the
    __slots__ and args to __init_ have `left_list` last.
    """

    __slots__ = ['augassign', 'expr', 'left']

    def __init__(self, *, augassign: ast.Astn, expr: Base, left: Base) -> None:
        # pylint: disable=super-init-not-called
        self.augassign = augassign
        self.expr = expr
        self.left = left

    def anchors(self, ctx: FqnCtx) -> Base:
        return AugAssignStmt(
            augassign=self.augassign,
            expr=_anchors_wrap(self.expr, ctx),
            left=_anchors_wrap(self.left, ctx))


class BreakStmt(EmptyBase):
    """Corresponds to `break_stmt`."""


class Class(Base):
    """Created by ClassDefStmt.anchors()."""

    __slots__ = ['fqn', 'name', 'bases']

    def __init__(
            self, *, fqn: Text, name: ast.Astn, bases: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn
        self.name = name
        self.bases = bases

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class ClassDefStmt(Base):
    """Corresponds to `classdef`."""

    __slots__ = ['name', 'bases', 'suite', 'scope_bindings']

    def __init__(self, *, name: 'NameBindsNode', bases: Sequence[Base],
                 suite: Base, scope_bindings: Mapping[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.bases = bases
        self.suite = suite
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Base:
        # Similar to FuncDefStmt.anchors
        class_fqn = ctx.fqn_dot + self.name.name.value
        class_fqn_dot = class_fqn + '.'
        name_anchors = xcast(NameBindsFqn, _anchors_wrap(
            self.name, ctx))  # already in bindings
        class_ctx = ctx._replace(
            fqn_dot=class_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, class_fqn_dot + name)
                                        for name in self.scope_bindings)),
            class_fqn=class_fqn,
            class_astn=self.name.name)
        class_anchors = Class(
            fqn=class_fqn,
            name=name_anchors.name,
            bases=[_anchors_wrap(base, ctx) for base in self.bases])
        return make_stmts([
            class_anchors, _anchors_wrap(self.suite, class_ctx)])


class CompForNode(Base):
    """Corresponds to `comp_for`.

    Note that for_exprlist isn't necessarily a list (e.g., to distinguish
    between `for foo= ...` and for foo,=...`)
    """

    __slots__ = [
        'for_astn', 'for_exprlist', 'in_testlist', 'comp_iter',
        'scope_bindings']

    def __init__(
            self, *, for_astn: ast.Astn, for_exprlist: Base, in_testlist: Base,
            comp_iter: Base, scope_bindings: Mapping[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.for_astn = for_astn
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.comp_iter = comp_iter
        self.scope_bindings = scope_bindings

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
        return ctx._replace(
            fqn_dot=for_fqn_dot,
            bindings=ctx.bindings.new_child(collections.OrderedDict()))

    def anchors(self, ctx: FqnCtx) -> Base:
        # Assume that the caller has created a new child in the
        # bindings, if needed.  This is done at the outermost level of
        # a comp_for (for Python 3), but not for any of the inner
        # comp_for's.
        # This handles the following:
        #    x for x in [1,x]  # `x` in `[1,x]` is outer scope
        #    (x, y) for x in [1,2] for y in range(x)  # `x` in `range(x)` is from `for x`
        # [(x, y) for x in [1,2,x] for y in range(x)]  # error: y undefined
        in_testlist_anchors = _anchors_wrap(self.in_testlist, ctx)
        ctx.bindings.update((name, ctx.fqn_dot + name)
                            for name in self.scope_bindings)
        for_exprlist_anchors = _anchors_wrap(self.for_exprlist, ctx)
        comp_iter_anchors = _anchors_wrap(self.comp_iter, ctx)
        return CompFor(
            for_astn=self.for_astn,
            for_exprlist=for_exprlist_anchors,
            in_testlist=in_testlist_anchors,
            comp_iter=comp_iter_anchors)


class CompFor(Base):
    """Created by CompForNode."""

    __slots__ = ['for_astn', 'for_exprlist', 'in_testlist', 'comp_iter']

    def __init__(self, *, for_astn: ast.Astn, for_exprlist: Base,
                 in_testlist: Base, comp_iter: Base) -> None:
        # pylint: disable=super-init-not-called
        self.for_astn = for_astn
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.comp_iter = comp_iter

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class CompIfCompIterNode(Base):
    """Corresponds to `comp_if` with `comp_iter`."""

    __slots__ = ['value_expr', 'comp_iter']

    def __init__(self, *, value_expr: Base, comp_iter: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_iter = comp_iter


class ContinueStmt(EmptyBase):
    """Corresponds to `continue_stmt`."""


class DecoratorsNode(ListBase):
    """Corresponds to `decorators`."""


class DecoratedStmt(ListBase):
    """Corresponds to `decorated`."""


class DecoratorDottedNameNode(ListBase):
    """Corresponds to `dotted_name` in `decorator` (see also DottedNameNode)."""

    def __init__(self, *, items: Sequence[Base]) -> None:
        # TODO: Delete this debugging-only __init__
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameRawNode, items)
        self.items = typing.cast(Sequence[NameRawNode], items)


class DecoratorNode(Base):
    """Corresponds to `decorator`.

    The grammar specifies the same `dotted_name` as `import_from` and
    `dotted_as_name` use, but we depend on this having been changed
    from DottedNameNode to DecoratorDottedNameNode.
    """

    __slots__ = ['name', 'args']

    def __init__(self, *, name: Base, args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(DecoratorDottedNameNode, name)
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomCallNode(
            atom=_anchors_wrap(self.name, ctx),
            args=[_anchors_wrap(arg, ctx) for arg in self.args])


class DelStmt(ListBase):
    """Corresponds to `del_stmt`."""


class DictKeyValue(ListBase):
    """Corresponds to `test ':' test` in DictGenListSetMakerCompForNode."""


class DictSetMakerNode(ListBase):
    """Corresponds to `dictsetmaker` without `comp_for`."""


class DictGenListSetMakerCompFor(Base):
    """Created by DictGenListSetMakerCompForNode.anchors()."""

    __slots__ = ['value_expr', 'comp_for']

    def __init__(self, *, value_expr: Base, comp_for: CompFor) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_for = comp_for

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class DictGenListSetMakerCompForNode(Base):
    """Corresponds to {`dict_set_maker', `listmaker`, testlist_gexp`} with
    `comp_for`. For our purposes, it's not important to know whether
    this is a list, set, or dict comprehension

    """

    __slots__ = ['value_expr', 'comp_for']

    def __init__(self, *, value_expr: Base, comp_for: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_for = xcast(CompForNode, comp_for)

    def anchors(self, ctx: FqnCtx) -> Base:
        comp_for_ctx = self.comp_for.scope_ctx(ctx)
        comp_for = xcast(CompFor, self.comp_for.anchors(comp_for_ctx))
        value_expr = self.value_expr.anchors(comp_for_ctx)
        return DictGenListSetMakerCompFor(
            value_expr=value_expr, comp_for=comp_for)


class DotNameTrailerNode(Base):
    """Corresponds to '.' NAME in trailer.

    This is only used when processing a trailer and is incorporated
    directly into AtomDotNode.
    """

    __slots__ = ['name', 'binds']

    # TODO: Remove binds and instead of DotNameTrailerNode{Binds,Ref}
    #       similar to Name{Binds,Ref}Fqn
    def __init__(self, *, name: Base, binds: bool) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(NameRawNode, name)
        self.binds = binds

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomDotNode(
            atom=atom, attr_name=self.name.name, binds=self.binds)

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class DottedNameNode(ListBase):
    """Corresponds to `dotted_name`."""

    def __init__(self, *, items: Sequence[Base]) -> None:
        # TODO: Delete this debugging-only __init__
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(NameRawNode, items)
        self.items = typing.cast(Sequence[NameRawNode], items)


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


class ExprStmt(Base):
    """Corresponds an expr-only from AssignMultipleExprStmt (q.v.)."""

    __slots__ = ['expr']

    def __init__(self, *, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr

    def anchors(self, ctx: FqnCtx) -> Base:
        # disallow reprocessing, even though probably benign (see AssignExprStmt).
        raise NotImplementedError(self)  # pragma: no cover


class ExceptClauseNode(Base):
    """Corresponds to `except_clause`."""

    __slots__ = ['expr', 'as_item']

    def __init__(self, *, expr: Base, as_item: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.as_item = as_item


class FileInput(Base):
    """Corresponds to `file_input`."""

    __slots__ = ['path', 'stmts', 'scope_bindings']

    def __init__(self, *, path: Text, stmts: Sequence[Base],
                 scope_bindings: Mapping[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.path = path
        self.stmts = stmts
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Base:
        file_ctx = ctx._replace(
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, ctx.fqn_dot + name)
                                        for name in self.scope_bindings)))
        stmts = [_anchors_wrap(stmt, file_ctx) for stmt in self.stmts]
        return FileInput(
            path=self.path, stmts=stmts, scope_bindings=self.scope_bindings)


class ForStmt(Base):
    """Corresponds to `for_stmt`.

    Note that for_exprlist isn't necessarily a list (e.g., to distinguish
    between `for foo= ...` and for foo,=...`)
    """

    __slots__ = ['for_exprlist', 'in_testlist', 'suite', 'else_suite']

    def __init__(self, *, for_exprlist: Base, in_testlist: Base, suite: Base,
                 else_suite: Base) -> None:
        # pylint: disable=super-init-not-called
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.suite = suite
        self.else_suite = else_suite

    def anchors(self, ctx: FqnCtx) -> Base:
        # Order is important: in_testlist is in outer bindings context,
        # for_exprlist adds to bindings, suite and else_suite use the
        # additional bindings (and also the bindings "leak" outside
        # the for-loop).
        in_testlist_anchors = _anchors_wrap(self.in_testlist, ctx)
        # for_exprlist adds to bindings
        for_exprlist_anchors = _anchors_wrap(self.for_exprlist, ctx)
        return ForStmt(
            for_exprlist=for_exprlist_anchors,
            in_testlist=in_testlist_anchors,
            suite=_anchors_wrap(self.suite, ctx),
            else_suite=_anchors_wrap(self.else_suite, ctx))


class Func(Base):
    """Created by FuncDefStmt.anchors()."""

    __slots__ = ['fqn', 'name', 'parameters', 'return_type']

    def __init__(self, *, fqn: Text, name: ast.Astn,
                 parameters: Sequence[Base], return_type: Base) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn
        self.name = name
        self.parameters = parameters
        self.return_type = return_type

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class FuncDefStmt(Base):
    """Corresponds to `funcdef` / `async_funcdef` or lambdadef.

    If it's a lambda, the `name` points to the `lambda` keyword and
    this can appear in an "expression" context, hence this is a
    subclass of Base and not of Base.
    """

    __slots__ = [
        'name', 'parameters', 'return_type', 'suite', 'scope_bindings']

    # _slot_anchors not needed because anchors() method is overriden.

    def __init__(self, *, name: 'NameBindsNode', parameters: Sequence[Base],
                 return_type: Base, suite: Base,
                 scope_bindings: Mapping[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.parameters = parameters
        self.return_type = return_type
        self.suite = suite
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Base:
        # Similar to ClassDefStmt.anchors
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
        name_anchors = xcast(NameBindsFqn, _anchors_wrap(self.name, ctx))
        func_ctx = ctx._replace(
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
                    name=_anchors_wrap(
                        xcast(TypedArgNode, self.parameters[0]).tname.name,
                        func_ctx),
                    type_expr=NameRefGenerated(fqn=ctx.class_fqn)),
                expr=OMITTED_NODE)
            parameters = [param0] + [
                xcast(TypedArgNode, _anchors_wrap(parameter, func_ctx))
                for parameter in self.parameters[1:]]
        else:
            parameters = [
                xcast(TypedArgNode, _anchors_wrap(parameter, func_ctx))
                for parameter in self.parameters]
        func_anchors = Func(
            fqn=func_fqn,
            name=name_anchors.name,
            parameters=parameters,
            return_type=_anchors_wrap(self.return_type, ctx))
        return make_stmts([func_anchors, _anchors_wrap(self.suite, func_ctx)])


class GlobalStmt(ListBase):
    """Corresponds to `global_stmt`."""


class IfStmt(ListBase):
    """Corresponds to `if_stmt`."""


class ImportAsNamesNode(ListBase):
    """Corresponds to `import_as_names`."""


class ImportDotNode(Base):
    """Corresponds to a DOT in `import_from`."""

    __slots__ = []  # type: Sequence[str]

    def __init__(self) -> None:
        # pylint: disable=super-init-not-called
        # TODO: test case
        pass


class ImportDottedAsNameFqn(Base):
    """Created by ImportDottedAsNameNode.anchors."""

    __slots__ = ['dotted_name', 'as_name']

    def __init__(self, *, dotted_name: Base, as_name: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_name = xcast(DottedNameNode, dotted_name)
        self.as_name = xcast(NameBindsFqn, as_name)

    def anchors(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


class ImportDottedAsNameNode(Base):
    """Corresponds to `dotted_as_name`.

    This is only used by `import_name`, so if the `as` is missing,
    then the first item in the `dotted_name` gets marked as "binds".
    """

    __slots__ = ['dotted_name', 'as_name']

    def __init__(self, *, dotted_name: Base, as_name: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_name = xcast(DottedNameNode, dotted_name)
        self.as_name = xcast(NameBindsNode, as_name)

    def anchors(self, ctx: FqnCtx) -> Base:
        return ImportDottedAsNameFqn(
            dotted_name=_anchors_wrap(self.dotted_name, ctx),
            as_name=_anchors_wrap(self.as_name, ctx))


class ImportDottedAsNamesFqn(ListBase):
    """Corresponds to `dotted_as_names`."""

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(
            ImportDottedAsNameFqn, items)  # TODO: remove
        self.items = typing.cast(Sequence[ImportDottedAsNameFqn], items)

    def anchors(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


class ImportDottedAsNamesNode(ListBase):
    """Created by ImportDottedAsNamesNode.anchors."""

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(
            ImportDottedAsNameNode, items)  # TODO: remove
        self.items = typing.cast(Sequence[ImportDottedAsNameNode], items)

    def anchors(self, ctx: FqnCtx) -> Base:
        return ImportDottedAsNamesFqn(
            items=[_anchors_wrap(item, ctx) for item in self.items])


class ImportFromStmt(Base):
    """Corresponds to `import_name`."""

    __slots__ = ['from_name', 'import_part']

    def __init__(
            self, *, from_name: Sequence[Base], import_part: Base) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance((DottedNameNode, ImportDotNode),
                                           from_name)  # TODO: remove
        self.from_name = typing.cast(Sequence[DottedNameNode], from_name)
        assert isinstance(
            import_part, (ImportAsNamesNode, StarNode))  # TODO: remove
        # TODO: self.import_part = typing.cast(Union[ImportAsNamesNode, StarNode], import_part)
        self.import_part = import_part

    def anchors(self, ctx: FqnCtx) -> Base:
        # ast_raw.cvt_import_from has made sure that the names in
        # import_part are NameBindsNode, so we don't need to do
        # anything special about them.
        return ImportFromStmt(
            from_name=[_anchors_wrap(name, ctx) for name in self.from_name],
            import_part=_anchors_wrap(self.import_part, ctx))


class ImportNameFqn(Base):
    """Created by ImportNameNode.anchors."""

    __slots__ = ['dotted_as_names']

    def __init__(self, *, dotted_as_names: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_as_names = xcast(ImportDottedAsNamesFqn, dotted_as_names)

    def anchors(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


class ImportNameNode(Base):
    """Corresponds to `import_name`."""

    __slots__ = ['dotted_as_names']

    def __init__(self, *, dotted_as_names: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_as_names = xcast(ImportDottedAsNamesNode, dotted_as_names)

    def anchors(self, ctx: FqnCtx) -> Base:
        return ImportNameFqn(
            dotted_as_names=_anchors_wrap(self.dotted_as_names, ctx))


class ListMakerNode(ListBase):
    """Corresponds to `listmaker` without `comp_for`."""


class NameBindsFqn(Base):
    """Created by NameBindsNode.anchors."""

    __slots__ = ['name', 'fqn']

    def __init__(self, name: ast.Astn, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.fqn = fqn

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class NameBindsNode(Base):
    """Corresponds to a NAME node, in binding context.

    Attributes:
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    __slots__ = ['name']

    def __init__(self, *, name: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.name = name

    def anchors(self, ctx: FqnCtx) -> Base:
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


class NameRawNode(Base):
    """Corresponds to a NAME node that doesn't get a FQN.

    This is just a wrapper for ast.Astn, so that ast_raw type-checking
    works (an alternative would be to convolute things so that
    ast.Astn is a subclass of ast_cooked.Base).

    Attributes:
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value

    """

    __slots__ = ['name']

    def __init__(self, *, name: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.name = name

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


class NameRefNode(Base):
    """Corresponds to a NAME node, in ref contet.

    Attributes:
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    __slots__ = ['name']

    def __init__(self, *, name: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.name = name

    def anchors(self, ctx: FqnCtx) -> Base:
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


class NameRefFqn(Base):
    """Created by NameRefNode.anchors."""

    __slots__ = ['name', 'fqn']

    def __init__(self, name: ast.Astn, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.fqn = fqn

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class NameRefGenerated(Base):
    """Like NameRef, but for `self` type nodes.
    This is used to distinguish between nodes that should generate
    anchors and those that are generated (e.g., for handling the
    `self` in method definitions and don't need anchors.
    """

    __slots__ = ['fqn']

    def __init__(self, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class NonLocalStmt(ListBase):
    """Corresponds to "nonlocal" variant of `global_stmt`."""


class NumberNode(Base):
    """Corresponds to a NUMBER node.

    Attributes:
    astn: The AST node of the number
    """

    __slots__ = ['astn']

    def __init__(self, *, astn: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.astn = astn

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


class OmittedNode(EmptyBase):
    """An item that is omitted (e.g., bases for a class)."""


# Singleton OmittedNode, to avoid creating many of them.
OMITTED_NODE = OmittedNode()


class OpNode(Base):
    """Corresponds to various expression nodes (unary, binary, comparison)."""

    __slots__ = ['op_astns', 'args']

    def __init__(self, *, op_astns: Sequence[ast.Astn],
                 args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(ast.Astn, op_astns)  # TODO: remove
        self.op_astns = op_astns
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Base:
        return OpNode(
            op_astns=self.op_astns,
            args=[_anchors_wrap(arg, ctx) for arg in self.args])


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


class StringNode(Base):
    """Corresponds to a STRING node.

    Attributes:
        astns: The AST nodes of the string
    """

    __slots__ = ['astns']

    def __init__(self, *, astns: Sequence[ast.Astn]) -> None:
        # pylint: disable=super-init-not-called
        self.astns = astns

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


class SubscriptNode(Base):
    """Corresponds to `subscript`."""

    __slots__ = ['expr1', 'expr2', 'expr3']

    def __init__(self, *, expr1: Base, expr2: Base, expr3: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3


class SubscriptListNode(Base):
    """Corresponds to `subscript_list`.

    This is only used when processing trailers and is incorporated
    directly into AtomSubscriptNode.
    """

    __slots__ = ['subscripts']

    def __init__(self, *, subscripts: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(
            SubscriptNode, subscripts)  # TODO: remove
        self.subscripts = typing.cast(Sequence[SubscriptNode], subscripts)

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomSubscriptNode(atom=atom, subscripts=self.subscripts)

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class TestListNode(ListBase):
    """Corresponds to ."""


class TnameNode(Base):
    """Corresponds to `tname`."""

    __slots__ = ['name', 'type_expr']

    def __init__(self, *, name: Base, type_expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.type_expr = type_expr


class TfpListNode(ListBase):
    """Corresponds to `tfplist`."""

    # TODO: test case (see ast_raw.cvt_tfplist)


class TryStmt(ListBase):
    """Corresponds to `try_stmt`."""


class TypedArgNode(Base):
    """Corresponds to `typedargslist` `tfpdef ['=' test]` and similar."""

    __slots__ = ['tname', 'expr']

    def __init__(self, *, tname: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.tname = xcast(TnameNode, tname)
        self.expr = expr


class TypedArgsListNode(Base):
    """Corresponds to `typedargslist`.

    This is only used when processing a funcdef; the args are given
    directly to FuncDefStmt, which is why anchors() isn't
    defined for TypedArgsListNode.
    """

    __slots__ = ['args']

    def __init__(self, *, args: Sequence[TypedArgNode]) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class WhileStmt(Base):
    """Corresponds to `while_stmt`."""

    __slots__ = ['test', 'suite', 'else_suite']

    def __init__(self, *, test: Base, suite: Base, else_suite: Base) -> None:
        # pylint: disable=super-init-not-called
        self.test = test
        self.suite = suite
        self.else_suite = else_suite


class WithItemNode(Base):
    """Corresponds to `with_item`."""

    __slots__ = ['item', 'as_item']

    def __init__(self, *, item: Base, as_item: Base) -> None:
        # pylint: disable=super-init-not-called
        self.item = item
        self.as_item = as_item


class WithStmt(Base):
    """Corresponds to `with_stmt`."""

    __slots__ = ['items', 'suite']

    def __init__(self, *, items: Sequence[Base], suite: Base) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(WithItemNode, items)  # TODO: remove
        self.items = typing.cast(Sequence[WithItemNode], items)
        self.suite = suite

    def anchors(self, ctx: FqnCtx) -> Base:
        return WithStmt(
            items=[_anchors_wrap(item, ctx) for item in self.items],
            suite=_anchors_wrap(self.suite, ctx))


class YieldNode(ListBase):
    """Corresponds to `yield_expr`."""

    # TODO: test case (see ast_raw.cvt_yield_expr, ast_raw.cvt_yield_stmt)


# === other facts that are output as JSON


class Meta(pod.PlainOldDataExtended):
    """Information about the file."""

    __slots__ = ['corpus', 'root', 'path', 'language', 'contents_b64']

    def __init__(self, *, corpus: Text, root: Text, path: Text, language: Text,
                 contents_b64: Text) -> None:
        # pylint: disable=super-init-not-called
        self.corpus = corpus
        self.root = root
        self.path = path
        self.language = language
        self.contents_b64 = contents_b64
