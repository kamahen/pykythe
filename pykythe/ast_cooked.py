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
from typing import Dict, Iterator, List, Text, Union

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
    """Corresponds to `ann_assign`."""

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

    def __init__(self, *, arguments: List['ArgNode']) -> None:
        # pylint: disable=super-init-not-called
        self.arguments = arguments

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

    def __init__(
            self, *, name: Union['NameNode', 'OmittedNode'],
            arg: Union['AtomTrailerNode', 'ComparisonOpNode', 'GenericNode',
                       'LambdaNode', 'NameNode', 'NumberNode', 'OpNode',
                       'StarExprNode', 'StarStarExprNode', 'StringNode'],
            comp_for: Union['CompForNode', 'OmittedNode']) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.arg = arg
        self.comp_for = comp_for

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

    def __init__(self, *, name: 'NameNode', as_name: 'NameNode') -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.as_name = as_name

    def fqns(self, ctx: FqnCtx) -> 'AsNameNode':
        return AsNameNode(
            name=self.name.fqns(ctx), as_name=self.as_name.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.as_name.anchors()


class AtomTrailerNode(AstNode):
    """Correponds to the atom, trailer part of power."""

    __slots__ = ('atom', 'trailers')

    def __init__(
            self, *, atom: Union['LambdaNode', 'NameNode'],
            trailers: List[Union['ArgListNode', 'DotNameTrailerNode',
                                 'GenericNode', 'SubscriptListNode']]) -> None:
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

    def __init__(self, *, op_astn: 'Leaf') -> None:
        # pylint: disable=super-init-not-called
        self.op_astn = op_astn

    def fqns(self, ctx: FqnCtx) -> 'AugAssignNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from ()


class ClassDefStmt(AstNode):
    """Corresponds to `classdef`."""

    __slots__ = ('name', 'bases', 'suite', 'bindings')

    def __init__(self, *, name: 'NameNode',
                 bases: Union['ArgListNode', 'OmittedNode'],
                 suite: 'GenericNode', bindings: Dict[Text, Text]) -> None:
        # pylint: disable=super-init-not-called
        # TODO: https://github.com/kamahen/pykythe/issues/1
        #       - document that `bindings` are for this level
        #         and above
        #       - do a pass over suite, collecting its bindings
        self.name = name
        self.bases = bases
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
        assert self.name.binds
        # TODO: add bases to ClassDefAnchor
        yield kythe.ClassDefAnchor(astn=self.name.astn, fqn=self.name.fqn)
        yield from self.bases.anchors()
        yield from self.suite.anchors()


class CompForNode(AstNode):
    """Corresponds to `comp_for`."""

    __slots__ = ('for_exprlist', 'in_testlist', 'comp_iter')

    def __init__(self, *, for_exprlist: Union['GenericNode', 'NameNode'],
                 in_testlist: Union['AtomTrailerNode', 'GenericNode',
                                    'NameNode', 'StringNode'],
                 comp_iter: Union['AtomTrailerNode', 'GenericNode', 'NameNode',
                                  'OmittedNode']) -> None:
        # pylint: disable=super-init-not-called
        self.for_exprlist = for_exprlist
        self.in_testlist = in_testlist
        self.comp_iter = comp_iter

    def fqns(self, ctx: FqnCtx) -> 'CompForNode':
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

    def __init__(self, *, op_astns: List[Text]) -> None:
        # pylint: disable=super-init-not-called
        self.op_astns = op_astns

    def fqns(self, ctx: FqnCtx) -> 'CompOpNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from ()


class ComparisonOpNode(AstNode):
    """Corresponds to `comparison_op`."""

    __slots__ = ('op', 'args')

    def __init__(self, *, op: 'CompOpNode',
                 args: List[Union['AtomTrailerNode', 'ComparisonOpNode',
                                  'NameNode', 'NumberNode']]) -> None:
        # pylint: disable=super-init-not-called
        self.op = op
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

    def __init__(self, *, name: 'DottedNameNode',
                 arglist: 'OmittedNode') -> None:
        # pylint: disable=super-init-not-called
        self.name = name
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

    def __init__(self, *, exprs: Union['GenericNode', 'NameNode']) -> None:
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
        yield from ()


class DotNameTrailerNode(AstNode):
    """Corresponds to '.' NAME in trailer."""

    __slots__ = ('name', )

    def __init__(self, *, name: 'NameNode') -> None:
        # pylint: disable=super-init-not-called
        self.name = name

    def fqns(self, ctx: FqnCtx) -> 'DotNameTrailerNode':
        return DotNameTrailerNode(name=self.name.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()


class DottedAsNameNode(AstNode):
    """Corresponds to `dotted_as_name`."""

    __slots__ = ('dotted_name', 'as_name')

    def __init__(self, *, dotted_name: 'DottedNameNode',
                 as_name: 'NameNode') -> None:
        # pylint: disable=super-init-not-called
        self.dotted_name = dotted_name
        self.as_name = as_name

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

    def __init__(self, *, names: List['DottedAsNameNode']) -> None:
        # pylint: disable=super-init-not-called
        self.names = names

    def fqns(self, ctx: FqnCtx) -> 'DottedAsNamesNode':
        return DottedAsNamesNode(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class DottedNameNode(AstNode):
    """Corresponds to `dotted_name`."""

    __slots__ = ('names', )

    def __init__(self, *, names: List['NameNode']) -> None:
        # pylint: disable=super-init-not-called
        self.names = names

    def fqns(self, ctx: FqnCtx) -> 'DottedNameNode':
        return DottedNameNode(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class ExprStmt(AstNode):
    """Corresponds to `expr_stmt`."""

    __slots__ = ('lhs', 'augassign', 'exprs')

    def __init__(self, *,
                 lhs: Union['AtomTrailerNode', 'GenericNode', 'NameNode'],
                 augassign: Union['AugAssignNode', 'OmittedNode'],
                 exprs: List[Union['AtomTrailerNode', 'GenericNode',
                                   'NameNode', 'NumberNode']]) -> None:
        # pylint: disable=super-init-not-called
        self.lhs = lhs
        self.augassign = augassign
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

    def __init__(self, *, exprlist: Union['GenericNode', 'NameNode'],
                 testlist: Union['AtomTrailerNode', 'GenericNode'],
                 suite: Union['AtomTrailerNode', 'ExprStmt', 'GenericNode'],
                 else_suite: Union['GenericNode', 'OmittedNode']) -> None:
        # pylint: disable=super-init-not-called
        self.exprlist = exprlist
        self.testlist = testlist
        self.suite = suite
        self.else_suite = else_suite

    def fqns(self, ctx: FqnCtx) -> 'ForStmt':
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
    """Corresponds to `funcdef` / `async_funcdef`."""

    __slots__ = ('name', 'parameters', 'return_type', 'suite', 'bindings')

    def __init__(self, *, name: 'NameNode',
                 parameters: Union['GenericNode', 'TypedArgsList'],
                 return_type: Union['NameNode', 'NumberNode', 'OmittedNode'],
                 suite: Union['AtomTrailerNode', 'ExprStmt', 'GenericNode',
                              'NameNode', 'NumberNode', 'OmittedNode'],
                 bindings: Dict[Text, Text]) -> None:
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
        func_fqn = ctx.fqn + '.' + self.name.astn.value + '.<local>'
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

    def __init__(self, *, names: List['NameNode']) -> None:
        # pylint: disable=super-init-not-called
        self.names = names

    def fqns(self, ctx: FqnCtx) -> 'GlobalStmt':
        return GlobalStmt(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class ImportAsNamesNode(AstNode):
    """Corresponds to `import_as_names`."""

    __slots__ = ('names', )

    def __init__(self, *, names: List['AsNameNode']) -> None:
        # pylint: disable=super-init-not-called
        self.names = names

    def fqns(self, ctx: FqnCtx) -> 'ImportAsNamesNode':
        return ImportAsNamesNode(
            names=[names_item.fqns(ctx) for names_item in self.names])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for names_item in self.names:
            yield from names_item.anchors()


class ImportFromStmt(AstNode):
    """Corresponds to `import_name`."""

    __slots__ = ('from_name', 'import_part')

    def __init__(self, *, from_name: List['DottedNameNode'],
                 import_part: Union['ImportAsNamesNode', 'StarNode']) -> None:
        # pylint: disable=super-init-not-called
        self.from_name = from_name
        self.import_part = import_part

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

    def __init__(self, *, dotted_as_names: 'DottedAsNamesNode') -> None:
        # pylint: disable=super-init-not-called
        self.dotted_as_names = dotted_as_names

    def fqns(self, ctx: FqnCtx) -> 'ImportNameNode':
        return ImportNameNode(dotted_as_names=self.dotted_as_names.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.dotted_as_names.anchors()


class LambdaNode(AstNode):

    __slots__ = ('args', 'expr')

    def __init__(self, *, args: Union['OmittedNode', 'TypedArgsList'],
                 expr: Union['AtomTrailerNode', 'GenericNode', 'NameNode',
                             'NumberNode', 'OpNode']) -> None:
        # pylint: disable=super-init-not-called
        self.args = args
        self.expr = expr

    def fqns(self, ctx: FqnCtx) -> 'LambdaNode':
        return LambdaNode(args=self.args.fqns(ctx), expr=self.expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.args.anchors()
        yield from self.expr.anchors()


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

    def __init__(self, *, binds: 'bool', astn: 'Leaf', fqn: Text) -> None:
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

    __slots__ = ('names', )

    def __init__(self, *, names: List['NameNode']) -> None:
        # pylint: disable=super-init-not-called
        self.names = names

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

    def __init__(self, *, astn: 'Leaf') -> None:
        # pylint: disable=super-init-not-called
        self.astn = astn

    def fqns(self, ctx: FqnCtx) -> 'NumberNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from ()


class OpNode(AstNode):
    """Corresponds to various expression nodes (unary, binary)."""

    __slots__ = ('op_astn', 'args')

    def __init__(
            # pylint: disable=super-init-not-called
            self,
            *,
            op_astn: 'Leaf',
            args: List[Union['AtomTrailerNode', 'ComparisonOpNode', 'NameNode',
                             'NumberNode', 'OpNode', 'StringNode']]) -> None:
        self.op_astn = op_astn
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

    def __init__(self, *, expr: Union['GenericNode', 'NumberNode']) -> None:
        # pylint: disable=super-init-not-called
        self.expr = expr

    def fqns(self, ctx: FqnCtx) -> 'StarExprNode':
        return StarExprNode(expr=self.expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.expr.anchors()


class StarStarExprNode(AstNode):
    """Corresponds to `'**' expr`."""

    __slots__ = ('expr', )

    def __init__(self, *, expr: 'GenericNode') -> None:
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
        yield from ()


class SubscriptListNode(AstNode):

    __slots__ = ('subscripts', )

    def __init__(self, *, subscripts: List['SubscriptNode']) -> None:
        # pylint: disable=super-init-not-called
        self.subscripts = subscripts

    def fqns(self, ctx: FqnCtx) -> 'SubscriptListNode':
        return SubscriptListNode(subscripts=[
            subscripts_item.fqns(ctx) for subscripts_item in self.subscripts
        ])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for subscripts_item in self.subscripts:
            yield from subscripts_item.anchors()


class SubscriptNode(AstNode):

    __slots__ = ('expr1', 'expr2', 'expr3')

    def __init__(self, *, expr1: Union['NameNode', 'NumberNode', 'OmittedNode',
                                       'OpNode', 'StringNode'],
                 expr2: Union['NumberNode', 'OmittedNode', 'OpNode'],
                 expr3: 'OmittedNode') -> None:
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

    __slots__ = ('name', 'type_expr')

    def __init__(
            self, *, name: 'NameNode',
            type_expr: Union['NameNode', 'NumberNode', 'OmittedNode', 'OpNode']
    ) -> None:
        # pylint: disable=super-init-not-called
        self.name = name
        self.type_expr = type_expr

    def fqns(self, ctx: FqnCtx) -> 'TnameNode':
        return TnameNode(
            name=self.name.fqns(ctx), type_expr=self.type_expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.type_expr.anchors()


class TfpListNode(AstNode):

    __slots__ = ('items', )

    def __init__(self, items) -> None:
        # pylint: disable=super-init-not-called
        self.items = items

    def fqns(self, ctx: FqnCtx) -> 'TfpListNode':
        return TfpListNode(items=[item.fqns(ctx) for item in self.items])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for item in self.items:
            yield from item.anchors()


class TypedArg(AstNode):

    __slots__ = ('name', 'expr')

    def __init__(
            # pylint: disable=super-init-not-called
            self,
            *,
            name: 'TnameNode',
            expr: Union['LambdaNode', 'NumberNode', 'OmittedNode']) -> None:
        self.name = name
        self.expr = expr

    def fqns(self, ctx: FqnCtx) -> 'TypedArg':
        return TypedArg(name=self.name.fqns(ctx), expr=self.expr.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.name.anchors()
        yield from self.expr.anchors()


class TypedArgsList(AstNode):

    __slots__ = ('args', )

    def __init__(self, *, args: List['TypedArg']) -> None:
        # pylint: disable=super-init-not-called
        self.args = args

    def fqns(self, ctx: FqnCtx) -> 'TypedArgsList':
        return TypedArgsList(
            args=[args_item.fqns(ctx) for args_item in self.args])

    def anchors(self) -> Iterator[kythe.Anchor]:
        for args_item in self.args:
            yield from args_item.anchors()


class WithItemNode(AstNode):

    __slots__ = ('item', 'as_item')

    def __init__(
            # pylint: disable=super-init-not-called
            self,
            *,
            item: 'AtomTrailerNode',
            as_item: Union['GenericNode', 'NameNode', 'OmittedNode']) -> None:
        self.item = item
        self.as_item = as_item

    def fqns(self, ctx: FqnCtx) -> 'WithItemNode':
        return WithItemNode(
            item=self.item.fqns(ctx), as_item=self.as_item.fqns(ctx))

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from self.item.anchors()
        yield from self.as_item.anchors()


class WithStmt(AstNode):

    __slots__ = ('items', 'suite')

    def __init__(self, *, items: List['WithItemNode'],
                 suite: 'GenericNode') -> None:
        # pylint: disable=super-init-not-called
        self.items = items
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
        yield from ()


class StarNode(AstNode):
    """Corresponds to `'*' expr`."""

    __slots__ = ()

    def fqns(self, ctx: FqnCtx) -> 'StarNode':
        return self

    def anchors(self) -> Iterator[kythe.Anchor]:
        yield from ()


# Singleton OmittedNode, to avoid creating many of them.
OMITTED_NODE = OmittedNode()
