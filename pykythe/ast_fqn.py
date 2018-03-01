"""Simplified "expressions" with FQNs, used to compute types.

For example, if we have in m.py:

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

To do this, we need to record all the assignment information in the
program and then compute types. In this example, we need to mark that
`self` in C.__init__ is of type `C` and so is `c` in the main program
(in both cases, the `f1` will refer to the same Kythe semantic node,
identified by `C.f1`). For the latter, we capture that `c` is defined
by the expression `call(id('m.C'))` -- when we "evaluate" this later,,
we see that `m.C` is defined by `class(C)`, and we can therefore
deduce that `c` is also of type `class(C)`.
"""

from typing import Any, Sequence, Set, Text  # pylint: disable=unused-import
from . import pod

# pylint: disable=too-few-public-methods


class Base(pod.PlainOldDataExtended):
    """Base class for expressions.

    This node should not be instantiated directly.
    """

    __slots__ = ()

    def __init__(self, **kwargs: Any) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not Base, "Must not instantiate ast_fqn.Base"
        for key, value in kwargs.items():
            setattr(self, key, value)


class ListBase(Base):
    """A generic class for holding multiple Base's."""

    __slots__ = ('items', )

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(
            self) is not ListBase, "Must not instantiate ast_fqn.ListBase"
        self.items = items


class AnnAssign(Base):
    """lhs: type = expr (with expr optional)."""

    __slots__ = ('lhs', 'lhs_type', 'expr')

    def __init__(self, *, lhs: Base, lhs_type: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.lhs_type = lhs_type
        self.expr = expr


class ArgList(Base):
    """from ast_coooked.ArgListNode."""  # TODO: remove?


class Assign(Base):
    """lhs1 = lhs2 = ... = expr"""

    __slots__ = ('lhs', 'expr')

    def __init__(self, *, lhs: Sequence[Base], expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.expr = expr


class AsName(Base):
    """An imported name."""

    __slots__ = ('name', 'as_name')

    def __init__(self, *, name: Text, as_name: Text) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.as_name = as_name


class AtomTrailer(Base):
    """An atom and trailers."""

    __slots__ = ('atom', 'trailers')

    def __init__(self, *, atom: Base, trailers: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.trailers = trailers


class AugAssign(Base):
    """lhs1 <op>= expr"""

    __slots__ = ('lhs', 'expr')

    def __init__(self, *, lhs: Base, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.expr = expr


class Call(Base):
    """Call an expression."""

    __slots__ = ('expr', )

    def __init__(self, *, expr: Base) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr


class ClassDef(Base):
    """A class definition."""

    __slots__ = ('name', 'bases', 'scope_bindings')

    def __init__(self, *, name: Text, bases: Sequence[Base],
                 scope_bindings: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.bases = bases
        self.scope_bindings = scope_bindings


class Colon(ListBase):
    """from ast_cooked.ColonNode."""


class Comparison(ListBase):
    """from ast_cooked.ComparisonNode."""


class CompFor(Base):
    """from ast_cooked.CompForNode."""

    __slots__ = ('for_exprlist', 'in_testlist', 'comp_iter')

    def __init__(self, *, for_exprlist: Base, in_testlist: Base,
                 comp_iter: Base) -> None:
        # pylint: disable=super-init-not-called
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.comp_iter = comp_iter


class CompIfCompIter(Base):
    """from ast_cooked.CompIfCompIterNode."""

    __slots__ = ('value_expr', 'comp_iter')

    def __init__(self, *, value_expr: Base, comp_iter: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value_expr = value_expr
        self.comp_iter = comp_iter


class Decorators(ListBase):
    """from ast_cooked.DecoratorsNode."""


class DottedName(Base):
    """from ast_cooked.DottedNameNode."""

    __slots__ = ('names', )

    def __init__(self, *, names: Sequence[Text]) -> None:
        # pylint: disable=super-init-not-called
        self.names = names


class Dict(Base):
    """A dictionary (e.g., from ast_cooked.DictSetMakerNode)."""

    __slots__ = ('value', )

    def __init__(self, *, value: Base) -> None:
        # pylint: disable=super-init-not-called
        self.value = value


class Dot(Base):
    """A "dot" operation (__getattr__, __setattr__, etc.)."""

    __slots__ = ('name', )

    def __init__(self, *, name: Text) -> None:
        # pylint: disable=super-init-not-called
        self.name = name


class EllipsisConst(Base):
    """An ellipsis (`...`) node."""


class ExprList(ListBase):
    """from ast_cooked.ExprListNode."""


class FuncDef(Base):
    """Function definition (e.g., a lambda."""

    __slots__ = ('fqn', 'return_type')

    def __init__(self, *, fqn: Text, return_type: Base) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn
        self.return_type = return_type


class ListExpr(ListBase):
    """from ast_cooked.ListExpr."""


class ListMaker(ListBase):
    """from ast_cooked.ListMakerNode."""


class Name(Base):
    """A single fully qualified name."""

    __slots__ = ('fqn', )

    def __init__(self, *, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn


class Number(Base):
    """A single number."""

    __slots__ = ('value', )

    def __init__(self, *, value: Text) -> None:
        # pylint: disable=super-init-not-called
        self.value = value


class Omitted(Base):
    """An omitted node."""


# Singleton Omitted, to avoid creating many of them.
OMITTED = Omitted()


class Op(ListBase):
    """An operator (from ast_cooked.OpNode)."""


class Subscript(ListBase):
    """subscript [item[1]:item[2]:item[3]]."""


class SubscriptList(ListBase):
    """from ast_cooked.SubscriptListNode."""


class String(Base):
    """A single string."""

    __slots__ = ('value', )

    def __init__(self, *, value: Text) -> None:
        # pylint: disable=super-init-not-called
        self.value = value


class Subscr(Base):
    """A subscription operation (__getitem__, __setitem__, next, etc.)."""

    __slots__ = ('atom', )

    def __init__(self, *, atom: Base) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom


class TestList(ListBase):
    """from ast_cooked.TestListNode."""


class TfpList(ListBase):
    """from ast_cooked.TfpListNode."""


class Yield(ListBase):
    """from ast_cooked.YieldNode."""


class Union(Base):
    """A union of types."""

    __slots__ = ('items', )

    def __init__(self, *, items: Sequence[Base]) -> None:
        # pylint: disable=super-init-not-called
        self.items = items


def make_union(items: Sequence[Base]) -> Base:
    new_items = set()  # type: Set[Base]
    for item in items:
        if isinstance(item, Union):
            new_items.update(item.items)
        else:
            new_items.add(item)
    if len(new_items) == 1:
        return list(new_items)[0]
    return Union(items=sorted(new_items))
