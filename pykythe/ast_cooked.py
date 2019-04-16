"""Nicer representation of AST nodes, for further processing.

ast_raw.cvt traverses an AST (in the lib2to3.pytree format) and puts
it into a more easily processed form, as defined in this module. While
traversing, it also marks binding and non-binding uses of all the
names (including handling of names that were marked "global" or
"nonlocal").

ast_cooked defines an add_fqns() method for all the nodes, which
resolves most names to fully qualified names (FQNs). Some nodes, when
processesd by add_fqns, produce a slightly different form cotaining
the FQN.

The basic usage is (assuming cooked_nodes was created by
ast_raw.cvt_parse_tree):
    add_fqns = ast_cooked.add_fqns(cooked_nodes, module, python_version)
    print(add_fqns.as_prolog_str())

Each node is a subclass of Base.

Here is an example of how these nodes are used to generate
cross-reference information. Suppose we have in m.py (annotated with
commented-out Kythe verifier comments, so that we can run tests on this
source code):

    #- // { @C defines/binding C=vname("m.C", _, _, "", python) }
    #- // { C.node/kind record }
    #- // { C.subkind class }
    class C:
      #- // { @self defines/binding Self=vname("m.C.self", _, _, "", python) }
      #- // // TODO: add links for self being of type C
      def __init__(self):
        #- // { @self ref Self }
        #- // { @f1 defines/binding C_F1=vname("m.C.f1", _, _, "", python) }
        #- // { C_F1 childof C }
        self.f1 = 0

    #- // { @C ref C }
    #- // { @c defines/binding VarC=vname("m.c", _, _, "", python) }
    c = C()
    #- // { @c ref VarC }
    #- // { @f1 ref C_F1 }
    print(c.f1)

To generate these facts, we need to record all the assignment
information in the program, resolve the names, and compute types
(which are also used for further name resolution). In this example, we
need to mark that `self` in C.__init__ is of type `C` and so is `c` in
the main program (in both cases, the `f1` will refer to the same Kythe
semantic node, identified by `C.f1`). For the latter, we capture that
`c` is defined by the expression `call(id('m.C'))` -- when we
"evaluate" this later, we see that `m.C` is defined by `class(C)`, and
we can therefore deduce that `c` is also of type `class(C)`.
"""

# from __future__ import annotations  # TODO: this upsets pytype, which can't handle Python 3.7
#       we can't make this conditional on sys.version_info > (3,7):
#       SyntaxError: from __future__ imports must occur at the beginning of the file
# The following needed to be quoted:
#    'Base'
#    'NameBindsNode'
#    'NameBindsFqn'
#    'NameRawNode'
#    'TypedArgNode'

import collections
import dataclasses
from dataclasses import dataclass
import functools
from typing import Any, Mapping, Iterable, List, Optional, Sequence, Text, TypeVar, Union
import typing

from . import ast, fakesys, pod, typing_debug
from .typing_debug import cast as xcast

# pylint: disable=too-few-public-methods
# pylint: disable-msg=too-many-arguments
# pylint: disable=too-many-lines


def add_fqns(cooked_nodes: 'Base', module: Text, python_version: int) -> 'Base':
    """Top-level wrapper for adding FQNs to the cooked AST."""
    return cooked_nodes.add_fqns(
        FqnCtx(fqn_dot=module + '.',
               bindings=collections.ChainMap(collections.OrderedDict()),
               class_fqn=None,
               class_astn=None,
               python_version=python_version))


@dataclass(frozen=True)
class FqnCtx(pod.PlainOldData):
    """Context for computing FQNs (fully qualified names).

    When a "scope" is entered (file, class, function, comprehension),
    we need to first scan the scope for bindings because of corner
    cases such as this:

    y = 1

    def foo(x):
        return x + y  # y is local
        y = 1         # because of this binding

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
    bindings: typing.ChainMap[Text, Text]
    class_fqn: Optional[Text]
    class_astn: Optional[ast.Astn]
    python_version: int
    __slots__ = ['fqn_dot', 'bindings', 'class_fqn', 'class_astn', 'python_version']

    def __post__init(self) -> None:
        assert self.python_version in (2, 3)


class Base(pod.PlainOldDataExtended):
    """Base class for data from AST nodes.

    These mostly correspond to nodes in lib2to3.pytree.{Node,Leaf},
    with some modifications. Some of them are generated from the
    add_fqns() method.

    This class must not be called directly.
    """

    # TODO: https://github.com/python/mypy/issues/4547

    __slots__ = []  # type: List[str]

    def __init__(self, **kwargs: Any) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not Base, ("Must not directly instantiate ast_cooked.Node")
        for attr, value in kwargs.items():
            setattr(self, attr, value)  # pragma: no cover

    def add_fqns(self, ctx: FqnCtx) -> 'Base':
        """Generate a new tree with FQNs filled in.

        This code defines the generic form of the `add_fqns` method,
        using self.__slots__.  In a few cases (e.g., those that have
        an attr that is Sequence[Base]), this method is overriden.

        A Kythe "anchor" is a pointer to a piece of source code
        (typically, a "name" of some kind in Python) to which semantic
        information is attached.

        In most cases, `add_fqns` merely recursively calls `add_fqns`
        on its contents and returns a new node that combines the
        results of the recursive calls to the contents.

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
          A new node that with names resolved to FQNs. Usually it is
          the same type as the original node but in a few cases (e.g.,
          NameBindsNode), something different is returned (e.g.,
          NameBindsFqn).
        """
        attr_values = {attr: self._attr_add_fqns(attr, ctx) for attr in self.__slots__}
        # TODO: https://github.com/python/mypy/issues/4602
        #       and then use self.__class__(**attr_values)
        return type(self)(**attr_values)

    def _attr_add_fqns(self, attr: Text, ctx: FqnCtx) -> 'Base':
        # TODO: inline this when fully debugged
        try:
            return xcast(Base, getattr(self, attr).add_fqns(ctx))
        except Exception as exc:
            raise RuntimeError('%r node=%r:%r' % (exc, attr, getattr(self, attr))) from exc


class BaseNoOutput(Base):
    """Base that is never output for further processing."""

    def as_prolog_str(self) -> str:
        return _not_implemented(self, '***ERROR***')


class BaseNoFqnProcessing(Base):
    """Base that doesn't implement add_fqns.

    Typically, this is because a node isn't reprocessed for FQNs. For
    example, it's created by a `add_fqns` method solely for output.
    """

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return _not_implemented(self, self)


class BaseNoFqnProcessingNoOutput(BaseNoOutput):
    """Base that doesn't implement add_fqns and isn't output.

    For example, the node is only used internally within as_raw.
    """

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return _not_implemented(self, self)


_T = TypeVar('_T')


def _not_implemented(obj: Base, fake_value: _T) -> _T:
    # The following code stops pylint abstract-method from triggering
    # in the classes that don't define it.
    if True:  # pylint: disable=using-constant-test
        raise NotImplementedError(obj)  # pragma: no cover
    return fake_value  # pragma: no cover


class BaseAtomTrailer(BaseNoFqnProcessingNoOutput):
    """Extension of Base for atom+trailer part of `power` (in ast_raw).

    Only for nodes that are the result of the grammar rule
    `power: [AWAIT] atom trailer* ['**' factor]`.

    This is a base class for DotNameTrailerNode, RawArgListNode, and
    RawSubscriptListNode.  The intent is that
    BaseAtomTrailer.atom_trailer_node(atom) will return an object that
    contains the atom and the trailer (AtomCallNode, AtomDotNode,
    AtomSubscriptNode).
    """

    def atom_trailer_node(self, atom: Base, binds: bool) -> Base:
        """For processing atom, trailer part of power (in ast_raw)."""
        raise NotImplementedError(self)  # pragma: no cover


@dataclass(frozen=True)
class ListBase(Base):
    """A convenience class for AST nodes (expr) that contain a single list.

    A ListBase node does not necessarily correspond to a repeated node
    in the grammar; for example, AssertStmt is a subclass of ListBase,
    with one or two items (the test and the optional argument to
    AssertionError).
    """

    items: Sequence[Base]
    __slots__ = ['items']

    def __post_init__(self) -> None:
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not ListBase, ("Must not directly instantiate ast_cooked.ListBase")

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # TODO: https://github.com/python/mypy/issues/4602
        #       and then use self.__class__(**attr_values)
        return type(self)(items=[item.add_fqns(ctx) for item in self.items])


@dataclass(frozen=True)
class EmptyBase(Base):
    """A convenience class for AST nodes (expr) that contain nothing."""

    __slots__ = []  # type: List[str]

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class RawAnnAssignNode(BaseNoFqnProcessingNoOutput):
    """Corresponds to `annassign` (expr can be OmittedNode).

    annassign: ':' test ['=' test]

    This is only used when processing an annassign statement;
    ast_raw.cvt_expr_stmt directly uses the contents of this node.
    """

    # TODO: also use for # type: ... comment
    # TODO: test case (see ast_raw.cvt_annassign)

    left_annotation: Base
    expr: Base
    __slots__ = ['left_annotation', 'expr']


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
class RawArgListNode(BaseAtomTrailer):
    """Corresponds to `arglist`.

    The arglist is only used when processing a `decorator`,
    `classdef`, or function call (`trailer`) and is incorporated
    directly into the appropriate node.
    """

    args: Sequence[Base]
    __slots__ = ['args']

    def atom_trailer_node(self, atom: Base, binds: bool) -> Base:
        # binds must be False, but don't check for this because
        # lib2to3/Grammar.txt allows it (the actual Python compiler
        # has an extra check that's not in the grammar).
        return AtomCallNode(atom=atom, args=self.args)


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
        return ArgumentNode(name=self.name, arg=self.arg.add_fqns(ctx))


@dataclass(frozen=True)
class AsNameNode(Base):
    """Corresponds to `import_as_name`."""

    name: Base
    as_name: Base
    __slots__ = ['name', 'as_name']


@dataclass(frozen=True)
class AssignExprStmt(BaseNoFqnProcessing):
    """Corresponds to a single assignment from AssignMultipleExprStmt (q.v.).

    Disallows reprocessing, which could change things if a `left`
    contains something that's in the `expr`.
    """

    left: Base
    expr: Base
    __slots__ = ['left', 'expr']


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
        expr = self.expr.add_fqns(ctx)
        # add_fqns(ctx) can modify the bindings in ctx, so order of
        # processing matters.
        left_add_fqns = list(reversed([item.add_fqns(ctx) for item in reversed(self.left_list)]))
        if len(left_add_fqns) == 1:
            return AssignExprStmt(left=left_add_fqns[0], expr=expr)
        if len(left_add_fqns) > 1:
            return make_stmts(AssignExprStmt(left=left, expr=expr) for left in left_add_fqns)
        return ExprStmt(expr=expr)


class AssertStmt(ListBase):
    """Corresponds to `assert_stmt`."""


@dataclass(frozen=True)
class AtomCallNode(Base):
    """Corresponds to `atom '(' [arglist] ')'`.

    The grammar allows an assignment to a function call, but this is
    illegal in the true Python grammar, so we just ignore that
    possibility.
    """

    atom: Base
    args: Sequence[Base]
    # binds: The grammar allows this, but it's illegal.
    __slots__ = ['atom', 'args']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AtomCallNode(atom=self.atom.add_fqns(ctx),
                            args=[arg.add_fqns(ctx) for arg in self.args])


@dataclass(frozen=True)
class AtomDotNode(Base):
    """Corresponds to `atom '.' NAME`.

    Unlike NameBindsFqn, NameRefFqn, we keep a bool to track whether
    this is in a binding context or not.
    """

    atom: Base
    attr_name: ast.Astn
    binds: bool
    __slots__ = ['atom', 'attr_name', 'binds']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AtomDotNode(atom=self.atom.add_fqns(ctx),
                           attr_name=self.attr_name,
                           binds=self.binds)


@dataclass(frozen=True)
class AtomSubscriptNode(Base):
    """Corresponds to `atom '[' [subscriptist] ']'`.

    Unlike NameBindsFqn, NameRefFqn, we keep a bool to track whether
    this is in a binding context or not.
    """

    atom: Base
    subscripts: Sequence[Base]
    binds: bool
    __slots__ = ['atom', 'subscripts', 'binds']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return AtomSubscriptNode(atom=self.atom.add_fqns(ctx),
                                 binds=self.binds,
                                 subscripts=[
                                     subscript.add_fqns(ctx) for subscript in self.subscripts])


@dataclass(frozen=True)
class AugAssignNode(BaseNoFqnProcessing):
    """Corresponds to `augassign`."""

    op: ast.Astn
    __slots__ = ['op']


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
        return AugAssignStmt(augassign=self.augassign,
                             expr=self.expr.add_fqns(ctx),
                             left=self.left.add_fqns(ctx))


class BreakStmt(EmptyBase):
    """Corresponds to `break_stmt`."""


@dataclass(frozen=True)
class Class(BaseNoFqnProcessing):
    """Created by ClassDefStmt.add_fqns()."""

    fqn: Text
    name: ast.Astn
    bases: Sequence[Base]
    __slots__ = ['fqn', 'name', 'bases']


@dataclass(frozen=True)
class ClassDefStmt(Base):
    """Corresponds to `classdef`."""

    name: 'NameBindsNode'
    bases: Sequence[Base]
    suite: Base
    scope_bindings: Mapping[Text, None]
    __slots__ = ['name', 'bases', 'suite', 'scope_bindings']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Similar to FuncDefStmt.add_fqns
        class_fqn = ctx.fqn_dot + self.name.name.value
        class_fqn_dot = class_fqn + '.'
        name_add_fqns = xcast(NameBindsFqn, self.name.add_fqns(ctx))  # already in bindings
        class_ctx = dataclasses.replace(
            ctx,
            fqn_dot=class_fqn_dot,
            bindings=ctx.bindings.new_child(
                collections.OrderedDict((name, class_fqn_dot + name)
                                        for name in self.scope_bindings)),
            class_fqn=class_fqn,
            class_astn=self.name.name)
        class_add_fqns = Class(fqn=class_fqn,
                               name=name_add_fqns.name,
                               bases=[base.add_fqns(ctx) for base in self.bases])
        return make_stmts([class_add_fqns, self.suite.add_fqns(class_ctx)])


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
    __slots__ = ['for_astn', 'for_exprlist', 'in_testlist', 'comp_iter', 'scope_bindings']

    def scope_ctx(self, ctx: FqnCtx) -> FqnCtx:
        """New FqnCtx for the scope of the comp_for (updates ctx for Python2).

        Used by DictGenListSetMakerCompForNode.
        """
        if ctx.python_version == 2:  # pragma: no cover
            # The bindings "leak" in Python2
            ctx.bindings.update((name, ctx.fqn_dot + name) for name in self.scope_bindings)
            return ctx
        for_fqn_dot = (f'{ctx.fqn_dot}<comp_for>'
                       f'[{self.for_astn.start:d},{self.for_astn.end:d}].')
        return xcast(
            FqnCtx,
            dataclasses.replace(ctx,
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
        in_testlist_add_fqns = self.in_testlist.add_fqns(ctx)
        ctx.bindings.update((name, ctx.fqn_dot + name) for name in self.scope_bindings)
        for_exprlist_add_fqns = self.for_exprlist.add_fqns(ctx)
        comp_iter_add_fqns = self.comp_iter.add_fqns(ctx)
        return CompFor(for_astn=self.for_astn,
                       for_exprlist=for_exprlist_add_fqns,
                       in_testlist=in_testlist_add_fqns,
                       comp_iter=comp_iter_add_fqns)


@dataclass(frozen=True)
class CompFor(BaseNoFqnProcessing):
    """Created by CompForNode."""

    for_astn: ast.Astn
    for_exprlist: Base
    in_testlist: Base
    comp_iter: Base
    __slots__ = ['for_astn', 'for_exprlist', 'in_testlist', 'comp_iter']


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
    """Corresponds to `dotted_name` in `decorator` (see also DottedNameNode).

    The caller will have converted the grammar's "raw" name to a
    NameRefNode (see ast_raw.cvt_decorator).
    """

    # TODO: the following should be NameRefNode for the first item and
    #       NameRawNode for the rest (in other words,
    #       DecoratorDottedNameNode shouldn't be a ListBase).
    items: List[Union['NameRefNode', 'NameRawNode']]

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[NameRawNode], items)
        assert isinstance(self.items, list)  # TODO: delete
        assert isinstance(self.items[0], NameRefNode)
        typing_debug.assert_all_isinstance(NameRawNode, self.items[1:])


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
        atom_reduced = functools.reduce(
            lambda atom, name: AtomDotNode(binds=False, atom=atom, attr_name=name.name),
            self.name.items[1:], self.name.items[0])
        return AtomCallNode(atom=atom_reduced.add_fqns(ctx),
                            args=[arg.add_fqns(ctx) for arg in self.args])


class DelStmt(ListBase):
    """Corresponds to `del_stmt`."""


class DictKeyValue(ListBase):
    """Corresponds to `test ':' test` in DictGenListSetMakerCompForNode."""


class DictSetMakerNode(ListBase):
    """Corresponds to `dictsetmaker` without `comp_for`."""


@dataclass(frozen=True)
class DictGenListSetMakerCompFor(BaseNoFqnProcessing):
    """Created by DictGenListSetMakerCompForNode.add_fqns()."""

    value_expr: Base
    comp_for: CompFor
    __slots__ = ['value_expr', 'comp_for']


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
        return DictGenListSetMakerCompFor(value_expr=value_expr, comp_for=comp_for)


class DottedNameNode(ListBase):
    """Corresponds to `dotted_name`."""

    items: List['NameRawNode']

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[NameRawNode], items)
        typing_debug.assert_all_isinstance(NameRawNode, self.items)


class EllipsisNode(EmptyBase):
    """Corresponds to `...`."""


class ExecStmt(ListBase):
    """Corresponds to `exec_stmt`."""


@dataclass(frozen=True)
class ExprListNode(Base):
    """Corresponds to `exprlist`, `testlist`, `testlist1`, `testlist_gexp`
    `testlist_star_expr` without `comp_for`.

    The `binds` attr isn't strictly needed, but it's been useful in
    finding some obscure bugs in how binding/ref context is computed.

    DelStmt doesn't use ExprListNode (despite the grammar); it
    directly stores the items (there's no semantic difference if the
    list ends with `,`).
    """

    items: Sequence[Base]
    binds: bool
    __slots__ = ['items', 'binds']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return ExprListNode(
            items=[item.add_fqns(ctx) for item in self.items],
            binds=self.binds,
        )


@dataclass(frozen=True)
class ExprStmt(BaseNoFqnProcessing):
    """Corresponds an expr-only from AssignMultipleExprStmt (q.v.).

    Doesn't allow reprocessing, even though probably benign (see AssignExprStmt).
    """

    expr: Base
    __slots__ = ['expr']


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
        stmts = [stmt.add_fqns(file_ctx) for stmt in self.stmts]
        return FileInput(path=self.path, stmts=stmts, scope_bindings=self.scope_bindings)


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
        in_testlist_add_fqns = self.in_testlist.add_fqns(ctx)
        # for_exprlist adds to bindings
        for_exprlist_add_fqns = self.for_exprlist.add_fqns(ctx)
        return ForStmt(for_exprlist=for_exprlist_add_fqns,
                       in_testlist=in_testlist_add_fqns,
                       suite=self.suite.add_fqns(ctx),
                       else_suite=self.else_suite.add_fqns(ctx))


@dataclass(frozen=True)
class Func(BaseNoFqnProcessing):
    """Created by FuncDefStmt.add_fqns()."""

    fqn: Text
    name: ast.Astn
    parameters: Sequence[Base]
    return_type: Base
    __slots__ = ['fqn', 'name', 'parameters', 'return_type']


@dataclass(frozen=True)
class FuncDefStmt(Base):
    """Corresponds to `funcdef` / `async_funcdef` or lambdadef.

    If it's a lambda, the `name` points to the `lambda` keyword and
    this can appear in an "expression" context, hence this is a
    subclass of Base and not of Base.
    """

    name: Union['NameBindsNode', 'NameBindsGlobalNode']
    parameters: Sequence[Base]
    return_type: Base
    suite: Base
    scope_bindings: Mapping[Text, None]
    __slots__ = ['name', 'parameters', 'return_type', 'suite', 'scope_bindings']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # Similar to ClassDefStmt.add_fqns
        # '.<local>.' is needed to distinguish `x` in following:
        #    def foo(x): pass
        #    foo.x = 'a string'
        if self.name.name.value == 'lambda':
            # Make a unique name for the lambda
            func_fqn = (f'{ctx.fqn_dot}<lambda>'
                        f'[{self.name.name.start:d},{self.name.name.end:d}]')
        else:
            func_fqn = f'{ctx.fqn_dot}{self.name.name.value}'
        func_fqn_dot = func_fqn + '.<local>.'
        # self.name is already in bindings
        name_add_fqns = xcast(NameBindsFqn, self.name.add_fqns(ctx))
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
        if (ctx.class_fqn and ctx.class_astn and self.parameters and isinstance(
                xcast(TypedArgNode, self.parameters[0]).tname.type_expr, OmittedNode) and
                isinstance(xcast(TypedArgNode, self.parameters[0]).expr, OmittedNode)):
            param0 = TypedArgNode(tname=TnameNode(name=xcast(
                TypedArgNode, self.parameters[0]).tname.name.add_fqns(func_ctx),
                                                  type_expr=NameRefGenerated(fqn=ctx.class_fqn)),
                                  expr=OMITTED_NODE)
            parameters = [param0] + [
                xcast(TypedArgNode, parameter.add_fqns(func_ctx))
                for parameter in self.parameters[1:]]
        else:
            parameters = [
                xcast(TypedArgNode, parameter.add_fqns(func_ctx)) for parameter in self.parameters]
        func_add_fqns = Func(fqn=func_fqn,
                             name=name_add_fqns.name,
                             parameters=parameters,
                             return_type=self.return_type.add_fqns(ctx))
        return make_stmts([func_add_fqns, self.suite.add_fqns(func_ctx)])


class GlobalStmt(ListBase):
    """Corresponds to `global_stmt`."""


@dataclass(frozen=True)
class IfStmt(Base):
    """Corresponds to `if_stmt`."""

    # eval_results are from evaluating the conditions (items[0,2,4,...])
    eval_results: Sequence[fakesys.EvalResult]
    items: Sequence[Base]  # the if-then-else parts (including the conditions)
    __slots__ = ['eval_results', 'items']

    def __post_init__(self) -> None:
        # There's always an else (possibly OmittedNode)
        assert len(self.items) == 2 * len(self.eval_results) + 1

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # TODO: https://github.com/python/mypy/issues/4602
        #       and then use self.__class__(**attr_values)
        return type(self)(eval_results=self.eval_results,
                          items=[item.add_fqns(ctx) for item in self.items])


class ImportAsNamesNode(ListBase):
    """Corresponds to `import_as_names`."""


@dataclass(frozen=True)
class ImportDotNode(Base):
    """Corresponds to a DOT in `import_from`.

    Attributes:
        dot: The AST node for the token ('.').
    """

    dot: ast.Astn
    __slots__ = ['dot']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class ImportDottedAsNameFqn(Base):
    """Created by ImportDottedAsNameNode.add_fqns (when there's an 'as').

    See ImportDottedFqn for the case when there's no 'as').
    """

    dotted_name: DottedNameNode
    as_name: 'NameBindsFqn'
    rest_names: List['NameBindsFqn']
    __slots__ = ['dotted_name', 'as_name', 'rest_names']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


@dataclass(frozen=True)
class ImportDottedAsNameNode(Base):
    """Corresponds to `dotted_as_name` (from `import_name`). """

    # TODO: new ast_cooked class ImportDottedNode for as_name=None

    dotted_name: DottedNameNode
    as_name: Optional[Union['NameBindsNode', 'NameBindsGlobalNode']]
    __slots__ = ['dotted_name', 'as_name']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        dotted_name = xcast(DottedNameNode, self.dotted_name.add_fqns(ctx))
        if self.as_name:
            top_name = xcast(NameBindsFqn, self.as_name.add_fqns(ctx))
        else:
            top_name = xcast(NameBindsFqn,
                             NameBindsNode(name=self.dotted_name.items[0].name).add_fqns(ctx))
        rest_names = []  # type: List['NameBindsFqn']
        rest_fqn = top_name.fqn
        for name in self.dotted_name.items[1:]:
            rest_fqn = rest_fqn + '.' + name.name.value
            rest_names.append(NameBindsFqn(fqn=rest_fqn, name=name.name))
        if self.as_name:
            return ImportDottedAsNameFqn(dotted_name=dotted_name,
                                         as_name=top_name,
                                         rest_names=rest_names)
        return ImportDottedFqn(dotted_name=dotted_name, top_name=top_name, rest_names=rest_names)


class ImportDottedAsNamesFqn(ListBase):
    """Corresponds to `dotted_as_names`."""

    items: Sequence[ImportDottedAsNameFqn]

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[ImportDottedAsNameFqn], items)
        typing_debug.assert_all_isinstance((ImportDottedAsNameFqn, ImportDottedFqn),
                                           self.items)  # TODO: delete

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


class ImportDottedAsNamesNode(ListBase):
    """Created by ImportDottedAsNamesNode.add_fqns."""

    items: Sequence[ImportDottedAsNameNode]

    def __post_init__(self) -> None:
        # self.items = typing.cast(Sequence[ImportDottedAsNameNode], items)
        typing_debug.assert_all_isinstance(ImportDottedAsNameNode, self.items)  # TODO: delete

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return ImportDottedAsNamesFqn(items=[item.add_fqns(ctx) for item in self.items])


@dataclass(frozen=True)
class ImportDottedFqn(Base):
    """Created by ImportDottedAsNameNode.add_fqns (when there's no 'as').

    This is only used by `import_name`. The statement "import foo.bar"
    creates a binding for "foo", which is in `top_name` (compare
    "import foo.bar as z", which binds "z" in
    ImportDottedAsNameNode.as_name).
    """

    dotted_name: DottedNameNode
    top_name: 'NameBindsFqn'
    rest_names: List['NameBindsFqn']
    __slots__ = ['dotted_name', 'top_name', 'rest_names']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self  # The components have already been processed


@dataclass(frozen=True)
class ImportFromStmt(Base):
    """Corresponds to `import_name`."""

    from_dots: Sequence[Base]
    from_name: Optional[Base]
    import_part: Base
    __slots__ = ['from_dots', 'from_name', 'import_part']

    def __post_init__(self) -> None:
        typing_debug.assert_all_isinstance(ImportDotNode, self.from_dots)
        assert (self.from_name is None or isinstance(self.from_name, DottedNameNode)), [self]
        # self.from_dots = typing.cast(Sequence[ImportDotNode], self.from_dots)
        # self.from_name = typing.cast(Sequence[DottedNameNode], self.from_name)
        assert isinstance(self.import_part, (ImportAsNamesNode, StarFqn, StarNode))
        # TODO: self.import_part = typing.cast(Union[ImportAsNamesNode, StarNode], import_part)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # ast_raw.cvt_import_from has made sure that the names in
        # import_part are NameBindsNode, so we don't need to do
        # anything special about them.
        # TODO: don't need add_fqns (nor for ImportDotNode, DottedNameNode)
        return ImportFromStmt(
            from_dots=[dot.add_fqns(ctx) for dot in self.from_dots],
            from_name=self.from_name.add_fqns(ctx) if self.from_name else self.from_name,
            import_part=self.import_part.add_fqns(ctx))


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
            dotted_as_names=xcast(ImportDottedAsNamesFqn, self.dotted_as_names.add_fqns(ctx)))


@dataclass(frozen=True)
class ListMakerNode(Base):
    """Corresponds to `listmaker` without `comp_for`.

    The `binds` attr isn't strictly needed, but it's been useful in
    finding some obscure bugs in how binding/ref context is computed.
    """

    items: Sequence[Base]
    binds: bool
    __slots__ = ['items', 'binds']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return ListMakerNode(
            items=[item.add_fqns(ctx) for item in self.items],
            binds=self.binds,
        )


@dataclass(frozen=True)
class NameBindsFqn(BaseNoFqnProcessing):
    """Created by NameBindsNode.add_fqns."""

    name: ast.Astn
    fqn: Text
    __slots__ = ['name', 'fqn']


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
        # it should be NAME=test).
        #
        # Other than that, an earlier pass will have already filled in
        # all the binding cases into ctx.bindings -- see
        # `scope_bindings` in ClassDefStmt, FuncDefStmt, FileInput,
        # CompForNode. ... the scope is scanned for all bindings, as
        # explained in the documentation for FqnCtx).
        if name in ctx.bindings:
            fqn = ctx.bindings[name] or ''
        else:
            fqn = ctx.fqn_dot + name
            ctx.bindings[name] = fqn
        return NameBindsFqn(name=self.name, fqn=fqn)


@dataclass(frozen=True)
class NameBindsGlobalNode(Base):
    """Like NameBindsNode, except for a nonlocal/global.

    The add_fqns method is essentially the same logic as for NameRefNode,
    except it returns a NameBindsFqn.

    Attributes:
        astn: The AST node of the name (a Leaf node) - the name is
              self.astn.value
    """

    name: ast.Astn
    __slots__ = ['name']

    # TODO: test case

    def add_fqns(self, ctx: FqnCtx) -> Base:
        # See also comments in NameRefNode.add_fqns
        name = self.name.value
        if name in ctx.bindings:
            return NameBindsFqn(name=self.name, fqn=ctx.bindings[name] or '')
        ctx.bindings[name] = ctx.fqn_dot + name
        return NameBindsUnknown(name=self.name, fqn_scope=ctx.fqn_dot[:-1])


@dataclass(frozen=True)
class NameBindsUnknown(BaseNoFqnProcessing):
    """Created by NameBindsGlobalNode.add_fqns."""

    name: ast.Astn
    fqn_scope: Text
    __slots__ = ['name', 'fqn_scope']


@dataclass(frozen=True)
class NameRawNode(Base):
    """Corresponds to a NAME node that doesn't get a FQN.

    This is just a wrapper for ast.Astn, so that ast_raw type-checking
    works (an alternative would be to convolute things so that
    ast.Astn is a subclass of ast_cooked.Base).

    Attributes:
        name: The AST node of the name (a Leaf node) - the name
              is self.astn.value
    """

    name: ast.Astn
    __slots__ = ['name']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class NameRefNode(Base):
    """Corresponds to a NAME node, in ref context.

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

        # Also, this code is a static *approximation* of Python's
        # dynamic name resolution. In a few cases, it won't do the
        # right thing, for example: if there's a `from foo import *`
        # inside a function definition. To which I say: too bad.
        if name in ctx.bindings:
            return NameRefFqn(name=self.name, fqn=ctx.bindings[name] or '')
        ctx.bindings[name] = ctx.fqn_dot + name
        return NameRefUnknown(name=self.name, fqn_scope=ctx.fqn_dot[:-1])


@dataclass(frozen=True)
class NameRefFqn(BaseNoFqnProcessing):
    """Created by NameRefNode.add_fqns."""

    name: ast.Astn
    fqn: Text
    __slots__ = ['name', 'fqn']


@dataclass(frozen=True)
class NameRefUnknown(BaseNoFqnProcessing):
    """Created by NameRefNode.add_fqns."""

    name: ast.Astn
    fqn_scope: Text
    __slots__ = ['name', 'fqn_scope']


@dataclass(frozen=True)
class NameRefGenerated(BaseNoFqnProcessing):
    """Like NameRef, but for `self` type nodes.

    This is used to distinguish between nodes that should generate
    add_fqns and those that are generated (e.g., for handling the
    type for `self` in method definitions) and don't need `add_fqns`.
    """

    fqn: Text
    __slots__ = ['fqn']


class NonLocalStmt(ListBase):
    """Corresponds to "nonlocal" variant of `global_stmt`."""


@dataclass(frozen=True)
class NumberComplexNode(Base):
    """Corresponds to a NUMBER(complex) node.

    Attributes:
    astn: The AST node of the number
    """

    astn: ast.Astn
    __slots__ = ['astn']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class NumberFloatNode(Base):
    """Corresponds to a NUMBER(float) node.

    Attributes:
    astn: The AST node of the number
    """

    astn: ast.Astn
    __slots__ = ['astn']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class NumberIntNode(Base):
    """Corresponds to a NUMBER(int) node.

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
OMITTED_NODE = OmittedNode()  # type: Base


@dataclass(frozen=True)
class OpNode(Base):
    """Corresponds to various expression nodes (unary, binary, comparison)."""

    op_astns: Sequence[ast.Astn]
    args: Sequence[Base]
    __slots__ = ['op_astns', 'args']

    def __post_init__(self) -> None:
        typing_debug.assert_all_isinstance(ast.Astn, self.op_astns)

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return OpNode(op_astns=self.op_astns, args=[arg.add_fqns(ctx) for arg in self.args])


class PassStmt(EmptyBase):
    """Corresponds to `pass_stmt`."""


class PrintStmt(ListBase):
    """Corresponds to `print_stmt`."""


class RaiseStmt(ListBase):
    """Corresponds to `raise_stmt`."""


@dataclass(frozen=True)
class StarFqn(Base):
    """Created by StarNode.add_fqns."""

    star: ast.Astn
    fqn: Text
    __slots__ = ['star', 'fqn']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


@dataclass(frozen=True)
class StarNode(Base):
    """Corresponds to `'*' (in from...import)`."""

    star: ast.Astn
    __slots__ = ['star']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return StarFqn(star=self.star, fqn=ctx.fqn_dot + self.star.value)


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
class StringBytesNode(Base):
    """Corresponds to a STRING node (bytes).

    Attributes:
        astns: The AST nodes of the byte string
    """

    astns: Sequence[ast.Astn]
    __slots__ = ['astns']

    def add_fqns(self, ctx: FqnCtx) -> Base:
        return self


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
class RawDotNameTrailerNode(BaseAtomTrailer):
    """Corresponds to '.' NAME in trailer.

    This is only used when processing a trailer and is incorporated
    directly into AtomDotNode.
    """

    name: NameRawNode
    __slots__ = ['name']

    def atom_trailer_node(self, atom: Base, binds: bool) -> Base:
        return AtomDotNode(atom=atom, attr_name=self.name.name, binds=binds)


@dataclass(frozen=True)
class RawSubscriptListNode(BaseAtomTrailer):
    """Corresponds to `subscript_list`.

    This is only used when processing trailers and is incorporated
    directly into AtomSubscriptNode.
    """

    subscripts: Sequence[Base]
    __slots__ = ['subscripts']

    def __post_init__(self) -> None:
        typing_debug.assert_all_isinstance(SubscriptNode, self.subscripts)
        # self.subscripts = typing.cast(Sequence[SubscriptNode], subscripts)

    def atom_trailer_node(self, atom: Base, binds: bool) -> Base:
        return AtomSubscriptNode(atom=atom, subscripts=self.subscripts, binds=binds)


@dataclass(frozen=True)
class RawTypedArgsListNode(BaseNoFqnProcessingNoOutput):
    """Corresponds to `typedargslist`.

    This is only used when processing a funcdef; the args are given
    directly to FuncDefStmt.
    """

    args: Sequence['TypedArgNode']
    __slots__ = ['args']


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
        return WithStmt(items=[item.add_fqns(ctx) for item in self.items],
                        suite=self.suite.add_fqns(ctx))


# === other facts that are output as Prolog terms.


@dataclass(frozen=True)
class Meta(pod.PlainOldDataExtended):
    """Information about the file."""

    kythe_corpus: Text
    kythe_root: Text
    path: Text
    language: Text
    contents_base64: Text
    contents: Text
    sha1: Text
    encoding: Text
    __slots__ = [
        'kythe_corpus', 'kythe_root', 'path', 'language', 'contents_base64', 'contents', 'sha1',
        'encoding']
