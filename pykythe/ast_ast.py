# For keeping notes about switching to asttokens and ast.py
# https://docs.python.org/3/library/ast.html
# https://github.com/gristlabs/asttokens
# src/cpython/Parser/Python.asdl

def dir_ast:
    # dir(ast)
    return \
        ['AST',
         'Add',
         'And',
         'AnnAssign',        #
         'Assert',           #
         'Assign',           #
         'AsyncFor',         #
         'AsyncFunctionDef', #
         'AsyncWith',        #
         'Attribute',
         'AugAssign',        #
         'AugLoad',
         'AugStore',
         'Await',
         'BinOp',
         'BitAnd',
         'BitOr',
         'BitXor',
         'BoolOp',
         'Break',            #
         'Bytes',
         'Call',
         'ClassDef',         #
         'Compare',
         'Constant',
         'Continue',         #
         'Del',
         'Delete',           #
         'Dict',
         'DictComp',
         'Div',
         'Ellipsis',
         'Eq',
         'ExceptHandler',
         'Expr',
         'Expression',
         'ExtSlice',
         'FloorDiv',
         'For',              #
         'FormattedValue',
         'FunctionDef',      #
         'GeneratorExp',
         'Global',           #
         'Gt',
         'GtE',
         'If',               #
         'IfExp',
         'Import',           #
         'ImportFrom',       #
         'In',
         'Index',
         'Interactive',
         'Invert',
         'Is',
         'IsNot',
         'JoinedStr',
         'LShift',
         'Lambda',
         'List',
         'ListComp',
         'Load',
         'Lt',
         'LtE',
         'MatMult',
         'Mod',
         'Module',
         'Mult',
         'Name',
         'NameConstant',
         'NodeTransformer',
         'NodeVisitor',
         'Nonlocal',         #
         'Not',
         'NotEq',
         'NotIn',
         'Num',
         'Or',
         'Param',
         'Pass',             #
         'Pow',
         'PyCF_ONLY_AST',
         'RShift',
         'Raise',            #
         'Return',           #
         'Set',
         'SetComp',
         'Slice',
         'Starred',
         'Store',
         'Str',
         'Sub',
         'Subscript',
         'Suite',
         'Try',              #
         'Tuple',
         'UAdd',
         'USub',
         'UnaryOp',
         'While',            #
         'With',             #
         'Yield',
         'YieldFrom',

         'alias',
         'arg',
         'arguments',
         'boolop',
         'cmpop',
         'comprehension',
         'copy_location',
         'dump',
         'excepthandler',
         'expr',
         'expr_context',
         'fix_missing_locations',
         'get_docstring',
         'increment_lineno',
         'iter_child_nodes',
         'iter_fields',
         'keyword',
         'literal_eval',
         'mod',
         'operator',
         'parse',
         'slice',
         'stmt',
         'unaryop',
         'walk',
         'withitem']
