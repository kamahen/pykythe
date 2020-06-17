#!/usr/bin/env python3.7
"""Main program for Python parser that outputs AST as Prolog term.

This is called by pykythe.pl, which further processes the AST.
"""

import argparse
import base64
from dataclasses import dataclass
import hashlib
import logging
import sys
import traceback
from lib2to3 import pytree
from lib2to3.pgen2 import parse as pgen2_parse
from lib2to3.pgen2 import tokenize as pgen2_tokenize
from typing import Iterable, Optional, Text, Tuple, Union

from . import ast, ast_raw, ast_cooked, ast_color, pod
from .typing_debug import cast as xcast

# TODO: a bit more refactoring - look at error types, for example.

RawBaseType = Union[ast_raw.Node, ast_raw.Leaf]


def main() -> int:
    """Main (uses sys.argv)."""
    src_file: Optional[ast.File]
    parse_error: Optional['CompilationError']
    parse_tree: Optional[RawBaseType]
    with_fqns: Union['CompilationError', ast_cooked.Base]
    pykythe_logger = logging.getLogger('pykythe')
    pykythe_logger_hdlr = logging.StreamHandler()
    pykythe_logger_hdlr.setFormatter(  # TODO: use something emacs *compilation* recognizes
            logging.Formatter('LOG %(asctime)s,%(msecs)03d-%(name)s-%(levelname)s: %(message)s',
                              datefmt='%H:%M:%S'))
    pykythe_logger.addHandler(pykythe_logger_hdlr)
    pykythe_logger.setLevel(logging.WARNING)

    # TODO: add to ast.File: args.root, args.corpus (even though they're in Meta)

    args = _get_args()
    pykythe_logger.info('Start parsing %s', args.srcpath)
    src_file, parse_error = _make_file(args)
    if src_file:
        assert not parse_error
        parse_tree, parse_error = _parse_file(src_file, args)
        if parse_tree:
            assert not parse_error
            parse_tree, with_fqns, parse_error = _process_ast(src_file, parse_tree, args)
        else:
            assert parse_error
            with_fqns = parse_error

        # b64encode returns bytes, so use decode() to turn it into a
        # string, because json.dumps can't process bytes -- it
        # processes UTF-8 encoded strings.
        meta = ast_cooked.Meta(kythe_corpus=args.kythe_corpus,
                               kythe_root=args.kythe_root,
                               path=args.srcpath,
                               language='python',
                               contents_base64=base64.b64encode(src_file.contents_bytes),
                               contents_str=src_file.contents_str,
                               contents_bytes=src_file.contents_bytes,
                               sha1=hashlib.sha1(src_file.contents_bytes).hexdigest(),
                               encoding=src_file.encoding)
    else:
        assert parse_error
        parse_tree = None
        pykythe_logger.error('pykythe.__main__.main: Parse error: %s',
                             parse_error)  # DO NOT SUBMIT - error message form
        with_fqns = parse_error
        with open(args.srcpath, 'rb') as src_f:
            contents_bytes = xcast(bytes, src_f.read())
        meta = ast_cooked.Meta(
                kythe_corpus=args.kythe_corpus,
                kythe_root=args.kythe_root,
                path=args.srcpath,
                language='python',
                contents_base64=base64.b64encode(contents_bytes),
                contents_str='',  # TODO: .decode('iso-8859-1') or .decode('utf-8', 'surrogateescape')
                contents_bytes=contents_bytes,
                sha1=hashlib.sha1(b'').hexdigest(),
                encoding='ascii')

    colored = ast_color.ColorFile(src_file, parse_tree, dict(with_fqns.name_astns())).color()

    with open(args.out_fqn_ast, 'w') as out_fqn_ast_file:
        pykythe_logger.debug('Output fqn= %r', out_fqn_ast_file)
        print(meta.as_prolog_str() + '.', file=out_fqn_ast_file)
        print(with_fqns.as_prolog_str() + '.', file=out_fqn_ast_file)
        print(ast_color.colored_list_as_prolog_str(colored) + '.', file=out_fqn_ast_file)
    pykythe_logger.debug('Finished')
    pykythe_logger.info('End parsing %s', args.srcpath)
    return 0


def _get_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Parse Python file, generating Kythe facts')
    # TODO: allow nargs='+' for multiple inputs?
    parser.add_argument('--srcpath', required=True, help='Input file')
    parser.add_argument('--module', required=True, help='FQN of module corresponding to --src')
    parser.add_argument('--out_fqn_ast',
                        required=True,
                        help=('output file for AST (with FQNs) in Python term format. '
                              'These are post-processed to further resolve names.'))
    parser.add_argument('--kythe_corpus',
                        dest='kythe_corpus',
                        default='',
                        help='Value of "corpus" in Kythe facts')
    parser.add_argument('--kythe_root',
                        dest='kythe_root',
                        default='',
                        help='Value of "root" in Kythe facts')
    parser.add_argument(  # TODO: version should be a triple -- see ast_raw.FAKE_SYS
            '--python_version',
            default=3,
            choices=[2, 3],
            type=int,
            help='Python major version')
    return parser.parse_args()


def _make_file(
        args: argparse.Namespace) -> Tuple[Optional[ast.File], Optional['CompilationError']]:
    parse_error: Optional['CompilationError']
    src_file: Optional[ast.File] = None
    try:
        src_file = ast.make_file(path=args.srcpath)
        parse_error = None
    except SyntaxError as exc:
        # TODO: This seems to sometimes be raised from an encoding error with
        #       msg='unknown encoding: ...', exc.lineno=None, exc.offset=None
        parse_error = ParseError(
                msg=str(exc),
                type='',
                context=str(('', (exc.lineno, exc.offset))) if exc.lineno or exc.offset else '',
                value=exc.text or '',
                srcpath=args.srcpath)
    except UnicodeDecodeError as exc:
        src_file = None
        # exc.object is the entire string, which can be determined
        # from the file
        parse_error = DecodeError(encoding=exc.encoding,
                                  start=exc.start,
                                  end=exc.end,
                                  reason=exc.reason,
                                  srcpath=args.srcpath)
    return src_file, parse_error


def _parse_file(src_file: ast.File,
                args: argparse.Namespace) -> Tuple[RawBaseType, Optional['CompilationError']]:
    parse_error: Optional['CompilationError'] = None
    parse_tree: Optional[RawBaseType] = None
    try:
        parse_tree = ast_raw.parse(src_file, args.python_version)
        parse_error = None
    except pgen2_tokenize.TokenError as exc:
        parse_error = ParseError(msg=str(exc), type='', context='', value='', srcpath=args.srcpath)
    except SyntaxError as exc:
        # TODO: This seems to sometimes be raised from an encoding error with
        #       msg='unknown encoding: ...', exc.lineno=None, exc.offset=None
        parse_error = ParseError(
                msg=str(exc),
                type='',
                context=str(('', (exc.lineno, exc.offset))) if exc.lineno or exc.offset else '',
                value=exc.text or '',
                srcpath=args.srcpath)
    except UnicodeDecodeError as exc:
        parse_error = DecodeError(encoding=exc.encoding,
                                  start=exc.start,
                                  end=exc.end,
                                  reason=exc.reason,
                                  srcpath=args.srcpath)
    except pgen2_parse.ParseError as exc:
        parse_error = ParseError(msg=str(exc),
                                 type=pytree.type_repr(exc.type),
                                 value=exc.value or '',
                                 context=str(exc.context),
                                 srcpath=args.srcpath)
    return parse_tree, parse_error


def _process_ast(
        src_file: ast.File, parse_tree: RawBaseType, args: argparse.Namespace
) -> Tuple[Optional[RawBaseType], Union['Crash', 'ParseError', ast_cooked.Base],
           Optional['ParseError']]:
    logging.getLogger('pykythe').debug('RAW= %r', parse_tree)
    new_parse_tree: Optional[RawBaseType]
    with_fqns: Union[ast_cooked.Base, 'ParseError', 'Crash']
    parse_error: Optional[Union['ParseError', Exception]]
    try:
        cooked_nodes = ast_raw.cvt_parse_tree(parse_tree, args.python_version, src_file)
        with_fqns = ast_cooked.add_fqns(cooked_nodes, args.module, args.python_version)
        parse_error = None
        new_parse_tree = parse_tree
    except pgen2_parse.ParseError as exc:
        parse_error = ParseError(msg=str(exc),
                                 type=pytree.type_repr(exc.type),
                                 value=exc.value or '',
                                 context=str(exc.context),
                                 srcpath=args.srcpath)
        # This can happen with, e.g. function def struct unpacking
        new_parse_tree = None
        with_fqns = parse_error
        logging.getLogger('pykythe').error('pykythe.__main__._process_ast: Parse error: %s',
                                           parse_error)  # DO NOT SUBMIT - error message form
    except Exception as exc:  # pylint: disable=broad-except
        # DO NOT SUBMIT: the parse_error assignment is probably incorrect
        parse_error = exc  # TODO: is this correct? we get this from an assertion check, for example
        new_parse_tree = parse_tree
        logging.getLogger('pykythe').error(
                'pykythe.__main__._process_ast: Caught error in cvt_parse_tree/with_fqns: %s %r',
                exc, exc)
        traceback.print_exc()
        with_fqns = Crash(str=str(exc), repr=repr(exc), srcpath=args.srcpath)
    return new_parse_tree, with_fqns, parse_error


class CompilationError(pod.PlainOldDataExtended):
    """Base error class that defines do-nothing pre_order, name_astns."""

    def pre_order(self) -> Iterable[pytree.Base]:  # pylint: disable=no-self-use
        yield from ()

    def name_astns(self) -> Iterable[Tuple[ast.Astn, str]]:  # pylint: disable=no-self-use
        """Do-nothing version of ast_cooked.Base.name_astns()."""
        yield from ()


@dataclass(frozen=True)
class DecodeError(CompilationError):
    """Encapsulate UnicodeDecodeError."""

    encoding: str
    start: int
    end: int
    reason: str
    srcpath: str
    __slots__ = ['encoding', 'start', 'end', 'reason', 'srcpath']


@dataclass(frozen=True)
class ParseError(CompilationError):
    """Encapsulate parse.ParseError."""

    msg: str
    type: str  # pytree.type_repr(type)
    value: Optional[str]
    context: str  # str(context)
    srcpath: str
    __slots__ = ['msg', 'type', 'value', 'context', 'srcpath']


@dataclass(frozen=True)
class Crash(CompilationError):
    """Encapsulate general exeception."""

    str: Text
    repr: Text
    srcpath: Text
    __slots__ = ['str', 'repr', 'srcpath']


if __name__ == '__main__':
    if sys.version_info < (3, 6):
        raise RuntimeError(f'Version must be 3.6 or later: {sys.version_info}')
    sys.setrecursionlimit(2000)  # default is 1000; more is needed for pod._as_prolog_str()
    sys.exit(main())
