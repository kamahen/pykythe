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
from lib2to3 import pytree
from typing import (  # pylint: disable=unused-import
    Any, Callable, Mapping, MutableMapping, Iterable, List, Optional, Sequence,
    Text)
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

    These mostly correspond to nodes in lib2to3.pytree.{Node,Leaf},
    with some modifications. Some of them are generated from the
    anchors() method.

    This class  should not be called directly.
    """

    # TODO: https://github.com/python/mypy/issues/4547

    __slots__ = ()  # type: Sequence[str]

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
        return self.__class__(  # type: ignore  # TODO: https://github.com/python/mypy/issues/4602
            **{
                attr: getattr(self, attr).anchors(ctx)
                for attr in self.__slots__
            })

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

    __slots__ = ('items', )

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not ListBase, (
            "Must not instantiate ast_cooked.ListBase")
        self.items = items

    def anchors(self, ctx: FqnCtx) -> Base:
        return self.__class__(  # type: ignore  # TODO: https://github.com/python/mypy/issues/4602
            items=[item.anchors(ctx) for item in self.items])


class AnnAssignNode(Base):
    """Corresponds to `annassign` (expr can be OmittedNode).

    This is only used when processing an annassign statement;
    ast_raw.cvt_expr_stmt directly uses the contents of this node.
    """

    # TODO: also use for # type: ... comment

    __slots__ = ('lhs_type', 'expr')

    def __init__(self, *, lhs_type: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        # TODO: test case (see ast_raw.cvt_annassign)
        self.lhs_type = lhs_type
        self.expr = expr

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class AnnAssignStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr annassign`.

    `expr` can be OmittedNode

    For anchors, `lhs` is processed after `lhs_type`,`expr` (which is
    why the __slots__ and args to __init_ have `lhs` last)
    """

    __slots__ = ('lhs_type', 'expr', 'lhs')

    def __init__(self, *, lhs_type: Base, expr: Base, lhs: Base) -> None:
        # pylint: disable=super-init-not-called
        # TODO: test case (see ast_raw.cvt_expr_stmt)
        self.lhs_type = lhs_type
        self.expr = expr
        self.lhs = lhs


class ArgListNode(Base):
    """Corresponds to `arglist`.

    This is only usesd when processing a `decorator`, `classdef`, or
    `trailers`, and is incorporated directly into the appropriate node.
    """

    __slots__ = ('args', )

    def __init__(self, *, args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomCallNode(atom=atom, args=self.args)

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class ArgumentNode(Base):
    """Corresponds to `argument: test '=' test`."""

    __slots__ = ('name', 'arg')

    def __init__(self, *, name: ast.Astn, arg: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.arg = arg

    def anchors(self, ctx: FqnCtx) -> Base:
        return ArgumentNode(name=self.name, arg=self.arg.anchors(ctx))


class AsNameNode(Base):
    """Corresponds to `import_as_name`."""

    __slots__ = ('name', 'as_name')

    def __init__(self, *, name: Base, as_name: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.as_name = as_name


class AssignExprStmt(Base):
    """Corresponds to `expr_stmt: testlist_star_expr ('=' (yield_expr|testlist_star_expr))*`.

    `lhs` is a list of assignment bindings; it can be empty.
    """

    __slots__ = ('lhs', 'expr')

    def __init__(self, *, lhs: Sequence[Base], expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.expr = expr

    def anchors(self, ctx: FqnCtx) -> Base:
        # anchors(ctx) can modify the bindings in ctx, so need to be
        # careful to do things in the right order.
        lhs_anchors = list(
            reversed([item.anchors(ctx) for item in reversed(self.lhs)]))
        return AssignExprStmt(lhs=lhs_anchors, expr=self.expr.anchors(ctx))


class AssertStmt(ListBase):
    """Corresponds to `assert_stmt`."""


def atom_trailer_node(atom: Base, trailers: Sequence[Base]) -> Base:
    """Create the appropriate AtomXXX nodes."""
    return functools.reduce(
        lambda atom, trailer: trailer.atom_trailer_node(atom), trailers, atom)


class AtomCallNode(Base):
    """Corresponds to `atom '(' [arglist] ')'`."""

    __slots__ = ('atom', 'args')

    def __init__(self, *, atom: Base, args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomCallNode(
            atom=self.atom.anchors(ctx),
            args=[arg.anchors(ctx) for arg in self.args])


class AtomDotNode(Base):
    """Corresponds to `atom '.' NAME`."""

    __slots__ = ('atom', 'attr_name')

    def __init__(self, *, atom: Base, attr_name: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.attr_name = attr_name

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomDotNode(
            atom=self.atom.anchors(ctx), attr_name=self.attr_name)


class AtomSubscriptNode(Base):
    """Corresponds to `atom '[' [subscriptist] ']'`."""

    __slots__ = ('atom', 'subscripts')

    def __init__(self, *, atom: Base, subscripts: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.subscripts = subscripts

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomSubscriptNode(
            atom=self.atom.anchors(ctx),
            subscripts=[
                subscript.anchors(ctx) for subscript in self.subscripts
            ])


class AugAssignNode(Base):
    """Corresponds to `augassign`."""

    __slots__ = ('op', )

    def __init__(self, *, op: ast.Astn) -> None:  # pylint: disable=invalid-name
        # pylint: disable=super-init-not-called
        self.op = op  # pylint: disable=invalid-name

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class AugAssignStmt(Base):
    """Corresponds to expr_stmt: augassign (yield_expr|testlist)."""

    __slots__ = ('lhs', 'augassign', 'expr')

    def __init__(self, *, lhs: Base, augassign: ast.Astn, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.augassign = augassign
        self.expr = expr

    def anchors(self, ctx: FqnCtx) -> Base:
        return AugAssignStmt(
            lhs=self.lhs.anchors(ctx),
            augassign=self.augassign,
            expr=self.expr.anchors(ctx))


class BreakStmt(ListBase):
    """Corresponds to `break_stmt`."""


class ClassDefStmt(Base):
    """Corresponds to `classdef`."""

    __slots__ = ('name', 'bases', 'suite', 'scope_bindings')

    def __init__(self, *, name: 'NameNode', bases: Sequence[Base], suite: Base,
                 scope_bindings: Mapping[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.bases = bases
        self.suite = suite
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Base:
        # Similar to FuncDefStmt.anchors
        assert self.name.binds
        class_fqn = ctx.fqn_dot + self.name.name.value
        class_fqn_dot = class_fqn + '.'
        name_anchors = xcast(
            NameBinds, self.name.anchors(ctx))  # already in bindings
        class_ctx = ctx._replace(
            fqn_dot=class_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, class_fqn_dot + name)
                                        for name in self.scope_bindings)))
        class_anchors = Class(
            fqn=class_fqn,
            name=name_anchors,
            bases=[base.anchors(ctx) for base in self.bases])
        # treat as: fqn = Class(...)
        return make_flattened_stmts([
            AssignExprStmt(lhs=[name_anchors], expr=class_anchors),
            self.suite.anchors(class_ctx)
        ])


class Class(Base):
    """Created by ClassDefStmt.anchors()."""

    __slots__ = ('fqn', 'name', 'bases')

    def __init__(self, *, fqn: Text, name: 'NameBinds',
                 bases: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn
        self.name = name
        self.bases = bases

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class ColonNode(ListBase):
    """Corresponds to `test ':' test` in DictGenListSetMakerCompForNode."""


class CompForNode(Base):
    """Corresponds to `comp_for`."""

    __slots__ = ('for_astn', 'for_exprlist', 'in_testlist', 'comp_iter',
                 'scope_bindings')

    def __init__(self, *, for_astn: ast.Astn, for_exprlist: Sequence[Base],
                 in_testlist: Base, comp_iter: Base,
                 scope_bindings: Mapping[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.for_astn = for_astn
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.comp_iter = comp_iter
        self.scope_bindings = scope_bindings

    def scope_ctx(self, ctx: FqnCtx) -> FqnCtx:
        """New FqnCtx for the scope of the comp_for (updates ctx for Python2)."""
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
        in_testlist_anchors = self.in_testlist.anchors(ctx)
        ctx.bindings.update((name, ctx.fqn_dot + name)
                            for name in self.scope_bindings)
        for_exprlist_anchors = [
            expr.anchors(ctx) for expr in self.for_exprlist
        ]
        comp_iter_anchors = self.comp_iter.anchors(ctx)
        return CompFor(
            for_astn=self.for_astn,
            for_exprlist=for_exprlist_anchors,
            in_testlist=in_testlist_anchors,
            comp_iter=comp_iter_anchors)


class CompFor(Base):
    """Created by CompForNode."""

    __slots__ = ('for_astn', 'for_exprlist', 'in_testlist', 'comp_iter')

    def __init__(self, *, for_astn: ast.Astn, for_exprlist: Sequence[Base],
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

    __slots__ = ('value_expr', 'comp_iter')

    def __init__(self, *, value_expr: Base, comp_iter: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_iter = comp_iter


class ContinueStmt(ListBase):
    """Corresponds to `continue_stmt`."""


class DecoratorsNode(ListBase):
    """Corresponds to `decorators`."""


class DecoratedStmt(ListBase):
    """Corresponds to `decorated`."""


class DecoratorNode(Base):
    """Corresponds to `decorator`."""

    __slots__ = ('name', 'args')

    def __init__(self, *, name: Base, args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(DottedNameNode, name)
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Base:
        return AtomCallNode(
            atom=self.name.anchors(ctx),
            args=[arg.anchors(ctx) for arg in self.args])


class DelStmt(ListBase):
    """Corresponds to `del_stmt`."""


class DictSetMakerNode(ListBase):
    """Corresponds to `dictsetmaker` without `comp_for`."""


class DictGenListSetMakerCompForNode(Base):
    """Corresponds to {`dict_set_maker', `listmaker`, testlist_gexp`} with
    `comp_for`. For our purposes, it's not important to know whether
    this is a list, set, or dict comprehension
    """

    __slots__ = ('value_expr', 'comp_for')

    def __init__(self, *, value_expr: Base, comp_for: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_for = comp_for


class DotNameTrailerNode(Base):
    """Corresponds to '.' NAME in trailer.

    This is only used when processing a trailer and is incorporated
    directly into AtomDotNode.
    """

    __slots__ = ('name', )

    def __init__(self, *, name: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.name = name

    def atom_trailer_node(self, atom: Base) -> Base:
        return AtomDotNode(atom=atom, attr_name=self.name)

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class DottedNameNode(ListBase):
    """Corresponds to `dotted_name`."""


class EllipsisNode(Base):
    """Corresponds to `...`."""


class ExecStmt(ListBase):
    """Corresponds to `exec_stmt`."""


class ExprListNode(ListBase):
    """Corresponds to `exprlist`.

    This is only used when processing del_stmt or for_stmt, which
    use the contents directly.
    """

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class ExceptClauseNode(Base):
    """Corresponds to `except_clause`."""

    __slots__ = ('expr', 'as_item')

    def __init__(self, *, expr: Base, as_item: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.as_item = as_item


class FileInput(Base):
    """Corresponds to `file_input`."""

    __slots__ = ('stmts', 'scope_bindings')

    def __init__(self, *, stmts: Sequence[Base],
                 scope_bindings: Mapping[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.stmts = stmts
        self.scope_bindings = scope_bindings

    def anchors(self, ctx: FqnCtx) -> Base:
        file_ctx = ctx._replace(
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, ctx.fqn_dot + name)
                                        for name in self.scope_bindings)))
        return FileInput(
            stmts=[stmt.anchors(file_ctx) for stmt in self.stmts],
            scope_bindings=self.scope_bindings)


class ForStmt(Base):
    """Corresponds to `for_stmt`."""

    __slots__ = ('for_exprlist', 'in_testlist', 'suite', 'else_suite')

    def __init__(self, *, for_exprlist: Sequence[Base], in_testlist: Base,
                 suite: Base, else_suite: Base) -> None:
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
        in_testlist_anchors = self.in_testlist.anchors(ctx)
        # for_exprlist adds to bindings
        for_exprlist_anchors = [
            expr.anchors(ctx) for expr in self.for_exprlist
        ]
        return ForStmt(
            for_exprlist=for_exprlist_anchors,
            in_testlist=in_testlist_anchors,
            suite=self.suite.anchors(ctx),
            else_suite=self.else_suite.anchors(ctx))


class FuncDefStmt(Base):
    """Corresponds to `funcdef` / `async_funcdef` or lambdadef.

    If it's a lambda, the `name` points to the `lambda` keyword and
    this can appear in an "expression" context, hence this is a
    subclass of Base and not of Base.
    """

    __slots__ = ('name', 'parameters', 'return_type', 'suite',
                 'scope_bindings')

    # _slot_anchors not needed because anchors() method is overriden.

    def __init__(self, *, name: 'NameNode', parameters: Sequence[Base],
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
        assert self.name.binds
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
        name_anchors = xcast(
            NameBinds, self.name.anchors(ctx))  # already in bindings
        func_ctx = ctx._replace(
            fqn_dot=func_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, func_fqn_dot + name)
                                        for name in self.scope_bindings)))
        func_anchors = Func(
            fqn=func_fqn,
            name=name_anchors,
            parameters=[
                parameter.anchors(ctx) for parameter in self.parameters
            ],
            return_type=self.return_type.anchors(func_ctx))
        # treat as : fqn = Func(...)
        return make_flattened_stmts([
            AssignExprStmt(lhs=[name_anchors], expr=func_anchors),
            self.suite.anchors(func_ctx)
        ])


class Func(Base):
    """Created by FuncDefStmt.anchors()."""

    __slots__ = ('fqn', 'name', 'parameters', 'return_type')

    def __init__(self, *, fqn: Text, name: 'NameBinds',
                 parameters: Sequence[Base], return_type: Base) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn
        self.name = name
        self.parameters = parameters
        self.return_type = return_type

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class GlobalStmt(ListBase):
    """Corresponds to `global_stmt`."""


class IfStmt(ListBase):
    """Corresponds to `if_stmt`."""


class ImportAsNamesNode(ListBase):
    """Corresponds to `import_as_names`."""


class ImportDotNode(Base):
    """Corresponds to a DOT in `import_from`."""

    __slots__ = ()

    def __init__(self) -> None:
        # pylint: disable=super-init-not-called
        # TODO: test case
        pass


class ImportDottedAsNameNode(Base):
    """Corresponds to `dotted_as_name`."""

    __slots__ = ('dotted_name', 'as_name')

    def __init__(self, *, dotted_name: Base, as_name: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_name = xcast(DottedNameNode, dotted_name)
        self.as_name = xcast(NameNode, as_name)
        assert self.as_name.binds


class ImportDottedAsNamesNode(ListBase):
    """Corresponds to `dotted_as_names`."""

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(
            ImportDottedAsNameNode, items)  # TODO: remove
        self.items = typing.cast(Sequence[ImportDottedAsNameNode], items)


class ImportFromStmt(Base):
    """Corresponds to `import_name`."""

    __slots__ = ('from_name', 'import_part')

    def __init__(
            self, *, from_name: Sequence[Base], import_part: Base) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(
            DottedNameNode, from_name)  # TODO: remove
        self.from_name = typing.cast(Sequence[DottedNameNode], from_name)
        assert isinstance(
            import_part, (ImportAsNamesNode, StarNode))  # TODO: remove
        # TODO: self.import_part = typing.cast(Union[ImportAsNamesNode, StarNode], import_part)
        self.import_part = import_part

    def anchors(self, ctx: FqnCtx) -> Base:
        return ImportFromStmt(
            from_name=self.from_name,
            import_part=self.import_part.anchors(ctx))


class ImportNameNode(Base):
    """Corresponds to `import_name`."""

    __slots__ = ('dotted_as_names', )

    def __init__(self, *, dotted_as_names: Base) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_as_names = xcast(ImportDottedAsNamesNode, dotted_as_names)

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


class ListMakerNode(ListBase):
    """Corresponds to `listmaker` without `comp_for`."""


class NameNode(Base):
    """Corresponds to a NAME node.

    Attributes:
        binds: Whether this name is in a binding context or not.
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    __slots__ = ('binds', 'name')

    def __init__(self, *, binds: bool, name: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.binds = binds
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
        if self.binds:
            return NameBinds(name=self.name, fqn=fqn)
        return NameRef(name=self.name, fqn=fqn)


class NameBinds(Base):
    """Created by NameNode with binds=True."""

    __slots__ = ('name', 'fqn')

    def __init__(self, name: ast.Astn, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.fqn = fqn

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not reprocessed.
        raise NotImplementedError(self)  # pragma: no cover


class NameRef(Base):
    """Created by NameNode with binds=False."""

    __slots__ = ('name', 'fqn')

    def __init__(self, name: ast.Astn, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
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

    __slots__ = ('astn', )

    def __init__(self, *, astn: ast.Astn) -> None:
        # pylint: disable=super-init-not-called
        self.astn = astn

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


class OmittedNode(Base):
    """An item that is omitted (e.g., bases for a class)."""

    __slots__ = ()

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


# Singleton OmittedNode, to avoid creating many of them.
OMITTED_NODE = OmittedNode()


class OpNode(Base):
    """Corresponds to various expression nodes (unary, binary, comparison)."""

    __slots__ = ('op_astns', 'args')

    def __init__(self, *, op_astns: Sequence[ast.Astn],
                 args: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(ast.Astn, op_astns)  # TODO: remove
        self.op_astns = op_astns
        self.args = args

    def anchors(self, ctx: FqnCtx) -> Base:
        return OpNode(
            op_astns=self.op_astns,
            args=[arg.anchors(ctx) for arg in self.args])


class PassStmt(ListBase):
    """Corresponds to `pass_stmt`."""


class PrintStmt(ListBase):
    """Corresponds to `print_stmt`."""


class RaiseStmt(ListBase):
    """Corresponds to `raise_stmt`."""


class StarNode(Base):
    """Corresponds to `'*' expr`."""


class Stmts(ListBase):
    """Corresponds to `simple_stmt`, `suite`."""


def make_flattened_stmts(items: Iterable[Base]) -> Stmts:
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

    __slots__ = ('astns', )

    def __init__(self, *, astns: Sequence[ast.Astn]) -> None:
        # pylint: disable=super-init-not-called
        self.astns = astns

    def anchors(self, ctx: FqnCtx) -> Base:
        return self


class SubscriptNode(Base):
    """Corresponds to `subscript`."""

    __slots__ = ('expr1', 'expr2', 'expr3')

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

    __slots__ = ('subscripts', )

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
    """Corresponds to `testlist`, `testlist1`, `testlist_gexp`
    `testlist_star_expr` without `comp_for`."""


class TnameNode(Base):
    """Corresponds to `tname`."""

    __slots__ = ('name', 'type_expr')

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
    """Corresponds to `typed_arg`."""

    __slots__ = ('name', 'expr')

    def __init__(self, *, name: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.name = xcast(TnameNode, name)
        self.expr = expr


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

    def anchors(self, ctx: FqnCtx) -> Base:
        # Not used anywhere
        raise NotImplementedError(self)  # pragma: no cover


class WhileStmt(Base):
    """Corresponds to `while_stmt`."""

    __slots__ = ('test', 'suite', 'else_suite')

    def __init__(self, *, test: Base, suite: Base, else_suite: Base) -> None:
        # pylint: disable=super-init-not-called
        self.test = test
        self.suite = suite
        self.else_suite = else_suite


class WithItemNode(Base):
    """Corresponds to `with_item`."""

    __slots__ = ('item', 'as_item')

    def __init__(self, *, item: Base, as_item: Base) -> None:
        # pylint: disable=super-init-not-called
        self.item = item
        self.as_item = as_item


class WithStmt(Base):
    """Corresponds to `with_stmt`."""

    __slots__ = ('items', 'suite')

    def __init__(self, *, items: Sequence[Base], suite: Base) -> None:
        # pylint: disable=super-init-not-called
        typing_debug.assert_all_isinstance(WithItemNode, items)  # TODO: remove
        self.items = typing.cast(Sequence[WithItemNode], items)
        self.suite = suite

    def anchors(self, ctx: FqnCtx) -> Base:
        return WithStmt(
            items=[item.anchors(ctx) for item in self.items],
            suite=self.suite.anchors(ctx))


class YieldNode(ListBase):
    """Corresponds to `yield_expr`."""

    # TODO: test case (see ast_raw.cvt_yield_expr, ast_raw.cvt_yield_stmt)


# === other facts that are output as JSON


class Meta(pod.PlainOldDataExtended):
    """Information about the file."""

    __slots__ = ('corpus', 'root', 'path', 'language', 'contents_b64')

    def __init__(self, *, corpus: Text, root: Text, path: Text, language: Text,
                 contents_b64: Text) -> None:
        # pylint: disable=super-init-not-called
        self.corpus = corpus
        self.root = root
        self.path = path
        self.language = language
        self.contents_b64 = contents_b64
