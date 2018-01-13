"""Representation of nodes, for further processing.

ast_raw.cvt traverses an AST (in the lib2to3.pytree format) and puts
it into a more easy to process form. While traversing, it also marks
binding and non-biding uses of all the namnes (including handling of
names that were marked "global" or "nonlocal").

Each node is a subclass of AstNode.
"""

import collections
import logging
from lib2to3 import pytree
from typing import cast, Dict, Iterator, List, Text, Union

from . import kythe, pod

# pylint: disable=too-few-public-methods
# pylint: disable-msg=too-many-arguments
# pylint: disable=too-many-lines


class FqnCtx(pod.PlainOldData):
    """Context for computing FQNs (fully qualified names).

    Attributes:
      fqn: The Fully Qualifed Name of this scope (module/function/class)
      bindings: mappings of names to FQNs at this scope
    """

    __slots__ = ('fqn', 'bindings')

    def __init__(self, *, fqn: Text, bindings: collections.ChainMap) -> None:
        # pylint: disable=super-init-not-called
        self.fqn = fqn
        self.bindings = bindings


class AstNode(pod.PlainOldData):
    """Base class for data from AST nodes.

    These correspond to nodes in lib2to3.pytree.{Node,Leaf}.

    Each node is intended to be non-mutable (there is no code for
    enforcing this, however).

    This node should not be called directly.
    """

    __slots__ = ()

    def __init__(self, *args, **kwargs) -> None:
        # pylint: disable=super-init-not-called
        # pylint: disable=unidiomatic-typecheck
        assert type(self) is not AstNode, "Must not instantiate AstNode"
        if args:
            raise ValueError('POD takes only kwargs')
        for key, value in kwargs.items():
            setattr(self, key, value)

    def fqns(self, ctx: FqnCtx) -> 'AstNode':
        """Make a new node with FQN information.

        The fully qualfied name (FQN) is a corpus-wide unique name for
        each "anchor" that gets a Kythe `ref` or `defines/binding`
        node. The FQN is a set of names separated by '.' that gives
        the name hierarchy. For examples, see the tests.

        This assumes that all subnodes of type NameNode have had the
        `binds` attribute set properly (ast_raw.cvt does this when
        creating each AstNode).

        Arguments:
          ctx: The context for generating the FQN information
               (mainly, the FQN of the enclosing scope).

        Returns:
          The same node, but with FQN information. See GenericNode for
          the canonical implementation.  A few nodes are special, such
          as FuncDefStmt and NameNode.
        """
        raise NotImplementedError(self)

    def anchors(self) -> Iterator[kythe.Anchor]:
        """Generate "anchor" nodes for Kythe facts.

        A Kythe "anchor" is a pointer to a piece of source code
        (typically, a "name" of some kind in Python) to which semantic
        information is attached. See GenericNode for the canonical
        implementation.  A few nodes are special, such as FuncDefStmt
        and NameNode.
        """
        raise NotImplementedError(self)


class GenericNode(AstNode):
    """An AST node that doesn't have any particular meaning.

    Some nodes are special, for example a function definition, which
    starts a new scope, or a name, which will produce either a
    `defines/binding` or `ref` Kythe fact. But many nodes have no
    particular meaning for generating Kythe facts, so they go into a a
    GenericNode.
    """

    __slots__ = ('descr', 'items')

    def __init__(self, *, descr: Text, items: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.descr = descr
        self.items = items

    def fqns(self, ctx: FqnCtx) -> 'GenericNode':
        return GenericNode(
            descr=self.descr, items=[item.fqns(ctx) for item in self.items])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for item in self.items:
            yield from item.anchors()


def make_generic_node(descr: Text, items: List[AstNode]) -> AstNode:
    """Create a GenericNode or use the single child.

    Arguments:
      descr: A comment for debugging that identifies the raw node used
             to create the GenericNode.
      items: The subnodes.

    Returns:
      If `items` is a single item, then returns that item; otherwise, returns
      a new GenericNode containing the subnodes (`items`.
    """
    return (items[0]
            if len(items) == 1 else GenericNode(descr=descr, items=items))


class AnnAssignNode(AstNode):
    """Corresponds to `annassign`."""

    __slots__ = ('expr', 'expr_type')

    def __init__(self, *, expr, expr_type) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr
        self.expr_type = expr_type

    def fqns(self, ctx: FqnCtx) -> 'AnnAssignNode':
        return AnnAssignNode(
            expr=self.expr.fqns(ctx), expr_type=self.expr_type.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors()
        yield from self.expr_type.anchors()


class ArgListNode(AstNode):
    """Corresponds to `arg_list`."""

    __slots__ = ('arguments', )

    def __init__(self, *, arguments: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.arguments = cast(List[ArgNode], arguments)

    def fqns(self, ctx: FqnCtx) -> 'ArgListNode':
        return ArgListNode(arguments=[
            arguments_item.fqns(ctx) for arguments_item in self.arguments
        ])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for arguments_item in self.arguments:
            yield from arguments_item.anchors()


class ArgNode(AstNode):
    """Corresponds to `argument`."""

    __slots__ = ('name', 'arg', 'comp_for')

    def __init__(self, *, name: AstNode, arg: AstNode,
                 comp_for: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = cast(Union[NameNode, OmittedNode], name)
        self.arg = arg
        self.comp_for = cast(Union[CompForNode, OmittedNode], comp_for)

    def fqns(self, ctx: FqnCtx) -> 'ArgNode':
        return ArgNode(
            name=self.name.fqns(ctx),
            arg=self.arg.fqns(ctx),
            comp_for=self.comp_for.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.arg.anchors()
        yield from self.comp_for.anchors()


class AsNameNode(AstNode):
    """Corresponds to `import_as_name`."""

    __slots__ = ('name', 'as_name')

    def __init__(self, *, name: AstNode, as_name: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = cast(NameNode, name)
        self.as_name = cast(NameNode, as_name)

    def fqns(self, ctx: FqnCtx) -> 'AsNameNode':
        return AsNameNode(
            name=self.name.fqns(ctx), as_name=self.as_name.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.as_name.anchors()


class AtomTrailerNode(AstNode):
    """Correponds to the atom, trailer part of power."""

    __slots__ = ('atom', 'trailers')

    def __init__(self, *, atom: AstNode, trailers: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.atom = atom
        self.trailers = trailers

    def fqns(self, ctx: FqnCtx) -> 'AtomTrailerNode':
        return AtomTrailerNode(
            atom=self.atom.fqns(ctx),
            trailers=[
                trailers_item.fqns(ctx) for trailers_item in self.trailers
            ])

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.atom.anchors()
        for trailers_item in self.trailers:
            yield from trailers_item.anchors()


class AugAssignNode(AstNode):
    """Corresponds to `augassign`."""

    __slots__ = ('op_astn', )

    def __init__(self, *, op_astn: pytree.Base) -> None:
        # pylint: disable=super-init-not-called
        self.op_astn = cast(pytree.Leaf, op_astn)

    def fqns(self, ctx: FqnCtx) -> 'AugAssignNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from []


class ClassDefStmt(AstNode):
    """Corresponds to `classdef`."""

    __slots__ = ('name', 'bases', 'suite', 'bindings')

    def __init__(self, *, name: AstNode, bases: AstNode, suite: AstNode,
                 bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = cast(NameNode, name)
        self.bases = cast(Union[ArgListNode, OmittedNode], bases)
        self.suite = suite
        self.bindings = bindings

    def fqns(self, ctx: FqnCtx) -> 'ClassDefStmt':
        class_fqn = ctx.fqn + '.' + self.name.astn.value
        class_ctx = ctx._replace(
            fqn=class_fqn,
            bindings=ctx.bindings.new_child(
                {name: class_fqn + '.' + name
                 for name in self.bindings}))
        return ClassDefStmt(
            name=self.name.fqns(ctx),
            bases=self.bases.fqns(ctx),
            suite=self.suite.fqns(class_ctx),
            bindings=self.bindings)

    def anchors(self) -> Iterator[kythe.Anchor]:
        # TODO: add bases to ClassDefAnchor
        assert self.name.binds
        yield kythe.ClassDefAnchor(astn=self.name.astn, fqn=self.name.fqn)
        yield from self.bases.anchors()
        yield from self.suite.anchors()


class CompForNode(AstNode):
    """Corresponds to `comp_for`."""

    __slots__ = ('for_exprlist', 'in_testlist', 'comp_iter')

    def __init__(self, *, for_exprlist: AstNode, in_testlist: AstNode,
                 comp_iter: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.comp_iter = comp_iter

    def fqns(self, ctx: FqnCtx) -> 'CompForNode':
        # TODO: Add self.for_expr's bindings to comp_iter (and for
        #       Python2 this "leaks" to current context). See also
        #       ForStmt.fqns
        return CompForNode(
            for_exprlist=self.for_exprlist.fqns(ctx),
            in_testlist=self.in_testlist.fqns(ctx),
            comp_iter=self.comp_iter.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.for_exprlist.anchors()
        yield from self.in_testlist.anchors()
        yield from self.comp_iter.anchors()


class CompOpNode(AstNode):
    """Corresponds to `comp_op`."""

    __slots__ = ('op_astns', )

    def __init__(self, *,
                 op_astns: List[Union[pytree.Node, pytree.Leaf]]) -> None:
        # pylint: disable=super-init-not-called
        self.op_astns = op_astns

    def fqns(self, ctx: FqnCtx) -> 'CompOpNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from []


class ComparisonOpNode(AstNode):
    """Corresponds to `comparison_op`."""

    __slots__ = ('op', 'args')

    def __init__(self, *, op: AstNode, args: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.op = cast(CompOpNode, op)
        self.args = args

    def fqns(self, ctx: FqnCtx) -> 'ComparisonOpNode':
        return ComparisonOpNode(
            op=self.op, args=[args_item.fqns(ctx) for args_item in self.args])

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.op.anchors()
        for args_item in self.args:
            yield from args_item.anchors()


class DecoratorNode(AstNode):
    """Corresponds to `decorator`."""

    __slots__ = ('name', 'arglist')

    def __init__(self, *, name: AstNode, arglist: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = cast(DottedNameNode, name)
        self.arglist = arglist

    def fqns(self, ctx: FqnCtx) -> 'DecoratorNode':
        return DecoratorNode(
            name=self.name.fqns(ctx), arglist=self.arglist.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.arglist.anchors()


class DelStmt(AstNode):
    """Corresponds to `del_stmt`."""

    __slots__ = ('exprs', )

    def __init__(self, *, exprs: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.exprs = exprs

    def fqns(self, ctx: FqnCtx) -> 'DelStmt':
        return DelStmt(exprs=self.exprs.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.exprs.anchors()


class DictSetMakerCompForNode(AstNode):
    """Corresponds to `dict_set_maker_comp_for`."""

    __slots__ = ('key_value_expr', 'comp_for')

    def __init__(self, *, key_value_expr, comp_for) -> None:
        # pylint: disable=super-init-not-called
        self.key_value_expr = key_value_expr
        self.comp_for = comp_for

    def fqns(self, ctx: FqnCtx) -> 'DictSetMakerCompForNode':
        return DictSetMakerCompForNode(
            key_value_expr=self.key_value_expr.fqns(ctx),
            comp_for=self.comp_for.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.key_value_expr.anchors()
        yield from self.comp_for.anchors()


class DotNode(AstNode):
    """Corresponds to a DOT in `import_from`."""

    __slots__ = ()

    def __init__(self) -> None:
        # pylint: disable=super-init-not-called
        pass

    def fqns(self, ctx: FqnCtx) -> 'DotNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from []


class DotNameTrailerNode(AstNode):
    """Corresponds to '.' NAME in trailer."""

    __slots__ = ('name', )

    def __init__(self, *, name: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = cast(NameNode, name)

    def fqns(self, ctx: FqnCtx) -> 'DotNameTrailerNode':
        return DotNameTrailerNode(name=self.name.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()


class DottedAsNameNode(AstNode):
    """Corresponds to `dotted_as_name`."""

    __slots__ = ('dotted_name', 'as_name')

    def __init__(self, *, dotted_name: AstNode, as_name: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_name = cast(DottedNameNode, dotted_name)
        self.as_name = cast(NameNode, as_name)

    def fqns(self, ctx: FqnCtx) -> 'DottedAsNameNode':
        return DottedAsNameNode(
            dotted_name=self.dotted_name.fqns(ctx),
            as_name=self.as_name.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.dotted_name.anchors()
        yield from self.as_name.anchors()


class DottedAsNamesNode(AstNode):
    """Corresponds to `dotted_as_names`."""

    __slots__ = ('names', )

    def __init__(self, *, names: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.names = cast(List[DottedAsNameNode], names)

    def fqns(self, ctx: FqnCtx) -> 'DottedAsNamesNode':
        return DottedAsNamesNode(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class DottedNameNode(AstNode):
    """Corresponds to `dotted_name`."""

    __slots__ = ('names', )

    def __init__(self, *, names: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.names = cast(List[NameNode], names)

    def fqns(self, ctx: FqnCtx) -> 'DottedNameNode':
        return DottedNameNode(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class ExprStmt(AstNode):
    """Corresponds to `expr_stmt`."""

    __slots__ = ('lhs', 'augassign', 'exprs')

    def __init__(self, *, lhs: AstNode, augassign: AstNode,
                 exprs: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.augassign = cast(Union[AugAssignNode, OmittedNode], augassign)
        self.exprs = exprs

    def fqns(self, ctx: FqnCtx) -> 'ExprStmt':
        return ExprStmt(
            lhs=self.lhs.fqns(ctx),
            augassign=self.augassign.fqns(ctx),
            exprs=[exprs_item.fqns(ctx) for exprs_item in self.exprs])

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.lhs.anchors()
        yield from self.augassign.anchors()
        for exprs_item in self.exprs:
            yield from exprs_item.anchors()


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

    def fqns(self, ctx: FqnCtx) -> 'ForStmt':
        # TODO: Add self.exprlist's bindings to suite and "leak" to
        #       outer context.  See also CompForNode.fqns
        return ForStmt(
            exprlist=self.exprlist.fqns(ctx),
            testlist=self.testlist.fqns(ctx),
            suite=self.suite.fqns(ctx),
            else_suite=self.else_suite.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.exprlist.anchors()
        yield from self.testlist.anchors()
        yield from self.suite.anchors()
        yield from self.else_suite.anchors()


class FuncDefStmt(AstNode):
    """Corresponds to `funcdef` / `async_funcdef` or lambdadef.

    If it's a lambda, the `name` points to the `lambda` keyword.
    """

    __slots__ = ('name', 'parameters', 'return_type', 'suite', 'bindings')

    def __init__(self, *, name: 'NameNode', parameters: AstNode,
                 return_type: AstNode, suite: AstNode,
                 bindings: Dict[Text, None]) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.parameters = parameters
        self.return_type = return_type
        self.suite = suite
        self.bindings = bindings

    def fqns(self, ctx: FqnCtx) -> 'FuncDefStmt':
        # '.<local>.' is needed to distinguish `x` in following:
        #    def foo(x): pass
        #    foo.x = 'a string'
        if self.name.astn.value == 'lambda':
            # Make a unique name for the lambda
            func_fqn = '{}.lambda[{:d},{:d}].<local>'.format(
                ctx.fqn, self.name.astn.lineno, self.name.astn.column)
        else:
            func_fqn = '{}.{}.<local>'.format(ctx.fqn, self.name.astn.value)
        # TODO: if inside class, use ctx.bindings.parents.new_child
        func_ctx = ctx._replace(
            fqn=func_fqn,
            bindings=ctx.bindings.new_child(
                {name: func_fqn + '.' + name
                 for name in self.bindings}))
        return FuncDefStmt(
            name=self.name.fqns(ctx),
            parameters=self.parameters.fqns(func_ctx),
            return_type=self.return_type.fqns(ctx),
            suite=self.suite.fqns(func_ctx),
            bindings=self.bindings)

    def anchors(self) -> Iterator[kythe.Anchor]:
        assert self.name.binds
        yield kythe.FuncDefAnchor(astn=self.name.astn, fqn=self.name.fqn)
        yield from self.parameters.anchors()
        yield from self.return_type.anchors()
        yield from self.suite.anchors()


class GlobalStmt(AstNode):
    """Corresponds to `global_stmt`."""

    __slots__ = ('names', )

    def __init__(self, *, names: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.names = cast(List[NameNode], names)

    def fqns(self, ctx: FqnCtx) -> 'GlobalStmt':
        return GlobalStmt(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class ImportAsNamesNode(AstNode):
    """Corresponds to `import_as_names`."""

    __slots__ = ('names', )

    def __init__(self, *, names: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.names = cast(List[AsNameNode], names)

    def fqns(self, ctx: FqnCtx) -> 'ImportAsNamesNode':
        return ImportAsNamesNode(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class ImportFromStmt(AstNode):
    """Corresponds to `import_name`."""

    __slots__ = ('from_name', 'import_part')

    def __init__(self, *, from_name: List[AstNode],
                 import_part: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.from_name = cast(List[DottedNameNode], from_name)
        self.import_part = cast(Union[ImportAsNamesNode, StarNode],
                                import_part)

    def fqns(self, ctx: FqnCtx) -> 'ImportFromStmt':
        return ImportFromStmt(
            from_name=[
                from_name_item.fqns(ctx) for from_name_item in self.from_name
            ],
            import_part=self.import_part.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        for from_name_item in self.from_name:
            yield from from_name_item.anchors()
        yield from self.import_part.anchors()


class ImportNameNode(AstNode):
    """Corresponds to `import_name`."""

    __slots__ = ('dotted_as_names', )

    def __init__(self, *, dotted_as_names: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.dotted_as_names = cast(DottedAsNamesNode, dotted_as_names)

    def fqns(self, ctx: FqnCtx) -> 'ImportNameNode':
        return ImportNameNode(dotted_as_names=self.dotted_as_names.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.dotted_as_names.anchors()


class NameNode(AstNode):
    """Corresponds to a NAME node.

    Attributes:
        binds: Whether this name is in a binding context or not.
        astn: The AST node of the name (a Leaf node) - the name
              is self.astn.value
        fqn: The Fully Qualified Name (FQN) for this name. Initially
             None; it is filled in by calling fqns() on the top node.
    """

    __slots__ = ('binds', 'astn', 'fqn')

    def __init__(self, *, binds: bool, astn: pytree.Leaf, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.binds = binds
        self.astn = astn
        self.fqn = fqn

    def fqns(self, ctx: FqnCtx) -> 'NameNode':
        name = self.astn.value
        if name in ctx.bindings:
            fqn = ctx.bindings[name]
        else:
            fqn = ctx.fqn + '.' + self.astn.value
            ctx.bindings[name] = fqn
        return NameNode(astn=self.astn, binds=self.binds, fqn=fqn)

    def anchors(self) -> Iterator[kythe.Anchor]:
        if self.binds:
            yield kythe.BindingAnchor(astn=self.astn, fqn=self.fqn)
        else:
            yield kythe.RefAnchor(astn=self.astn, fqn=self.fqn)


class NonLocalStmt(AstNode):
    """Corresponds to "nonlocal" variant of `global_stmt`."""

    __slots__ = ('names', )

    def __init__(self, *, names: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.names = cast(List[NameNode], names)

    def fqns(self, ctx: FqnCtx) -> 'NonLocalStmt':
        return NonLocalStmt(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class NumberNode(AstNode):
    """Corresponds to a NUMBER node.

    Attributes:
    astn: The AST node of the number
    """

    __slots__ = ('astn', )

    def __init__(self, *, astn: pytree.Leaf) -> None:
        # pylint: disable=super-init-not-called
        self.astn = astn

    def fqns(self, ctx: FqnCtx) -> 'NumberNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from []


class OpNode(AstNode):
    """Corresponds to various expression nodes (unary, binary)."""

    __slots__ = ('op_astn', 'args')

    def __init__(self, *, op_astn: pytree.Base, args: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.op_astn = cast(pytree.Leaf, op_astn)
        self.args = args

    def fqns(self, ctx: FqnCtx) -> 'OpNode':
        return OpNode(
            op_astn=self.op_astn,
            args=[args_item.fqns(ctx) for args_item in self.args])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for args_item in self.args:
            yield from args_item.anchors()


class StarExprNode(AstNode):
    """Corresponds to `star_expr`."""

    __slots__ = ('expr', )

    def __init__(self, *, expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr

    def fqns(self, ctx: FqnCtx) -> 'StarExprNode':
        return StarExprNode(expr=self.expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors()


class StarStarExprNode(AstNode):
    """Corresponds to `'**' expr`."""

    __slots__ = ('expr', )

    def __init__(self, *, expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr

    def fqns(self, ctx: FqnCtx) -> 'StarStarExprNode':
        return StarStarExprNode(expr=self.expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors()


class StringNode(AstNode):
    """Corresponds to a STRING node.

    Attributes:
        astns: The AST nodes of the string
    """

    __slots__ = ('values', 'astns')

    def __init__(self, *, astns: List[pytree.Leaf]) -> None:
        # pylint: disable=super-init-not-called
        self.astns = astns

    def fqns(self, ctx: FqnCtx) -> 'StringNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from []


class SubscriptListNode(AstNode):
    """Corresponds to `subscript_list`."""

    __slots__ = ('subscripts', )

    def __init__(self, *, subscripts: List[AstNode]) -> None:
        # pylint: disable=super-init-not-called
        self.subscripts = cast(List[SubscriptNode], subscripts)

    def fqns(self, ctx: FqnCtx) -> 'SubscriptListNode':
        return SubscriptListNode(subscripts=[
            subscripts_item.fqns(ctx) for subscripts_item in self.subscripts
        ])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for subscripts_item in self.subscripts:
            yield from subscripts_item.anchors()


class SubscriptNode(AstNode):
    """Corresponds to `subscript`."""

    __slots__ = ('expr1', 'expr2', 'expr3')

    def __init__(self, *, expr1: AstNode, expr2: AstNode,
                 expr3: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3

    def fqns(self, ctx: FqnCtx) -> 'SubscriptNode':
        return SubscriptNode(
            expr1=self.expr1.fqns(ctx),
            expr2=self.expr2.fqns(ctx),
            expr3=self.expr3.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.expr1.anchors()
        yield from self.expr2.anchors()
        yield from self.expr3.anchors()


class TnameNode(AstNode):
    """Corresponds to `tname`."""

    __slots__ = ('name', 'type_expr')

    def __init__(self, *, name: AstNode, type_expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = cast(NameNode, name)
        self.type_expr = type_expr

    def fqns(self, ctx: FqnCtx) -> 'TnameNode':
        return TnameNode(
            name=self.name.fqns(ctx), type_expr=self.type_expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.type_expr.anchors()


class TfpListNode(AstNode):
    """Corresponds to `tfplist`."""

    __slots__ = ('items', )

    def __init__(self, items) -> None:
        # pylint: disable=super-init-not-called
        self.items = items

    def fqns(self, ctx: FqnCtx) -> 'TfpListNode':
        return TfpListNode(items=[item.fqns(ctx) for item in self.items])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for item in self.items:
            yield from item.anchors()


class TypedArgNode(AstNode):
    """Corresponds to `typed_arg`."""

    __slots__ = ('name', 'expr')

    def __init__(self, *, name: AstNode, expr: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.name = cast(TnameNode, name)
        self.expr = expr

    def fqns(self, ctx: FqnCtx) -> 'TypedArgNode':
        return TypedArgNode(name=self.name.fqns(ctx), expr=self.expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.expr.anchors()


class TypedArgsListNode(AstNode):
    """Corresponds to `typedargslist`."""

    __slots__ = ('args', )

    def __init__(self, *, args: List[TypedArgNode]) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def fqns(self, ctx: FqnCtx) -> 'TypedArgsListNode':
        return TypedArgsListNode(
            args=[args_item.fqns(ctx) for args_item in self.args])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for args_item in self.args:
            yield from args_item.anchors()


class WithItemNode(AstNode):
    """Corresponds to `with_item`."""

    __slots__ = ('item', 'as_item')

    def __init__(self, *, item: AstNode, as_item: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.item = cast(AtomTrailerNode, item)
        self.as_item = as_item

    def fqns(self, ctx: FqnCtx) -> 'WithItemNode':
        return WithItemNode(
            item=self.item.fqns(ctx), as_item=self.as_item.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.item.anchors()
        yield from self.as_item.anchors()


class WithStmt(AstNode):
    """Corresponds to `with_stmt`."""

    __slots__ = ('items', 'suite')

    def __init__(self, *, items: List[AstNode], suite: AstNode) -> None:
        # pylint: disable=super-init-not-called
        self.items = cast(List[WithItemNode], items)
        self.suite = suite

    def fqns(self, ctx: FqnCtx) -> 'WithStmt':
        return WithStmt(
            items=[items_item.fqns(ctx) for items_item in self.items],
            suite=self.suite.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        for items_item in self.items:
            yield from items_item.anchors()
        yield from self.suite.anchors()


class OmittedNode(AstNode):
    """An item that is omitted (e.g., bases for a class)."""

    __slots__ = ()

    def fqns(self, ctx: FqnCtx) -> 'OmittedNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from []


class StarNode(AstNode):
    """Corresponds to `'*' expr`."""

    __slots__ = ()

    def fqns(self, ctx: FqnCtx) -> 'StarNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from []


# Singleton OmittedNode, to avoid creating many of them.
OMITTED_NODE = OmittedNode()
