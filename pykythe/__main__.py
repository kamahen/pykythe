#!/usr/bin/env python3.6
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
from typing import Optional, Text, Union

from .typing_debug import cast as xcast
from . import ast, ast_raw, ast_cooked, pod


def main() -> int:  # pylint: disable=too-many-statements
    """Main (uses sys.argv)."""
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
    args = parser.parse_args()

    src_file: Optional[ast.File] = None
    parse_error: Optional[Union[DecodeError, ParseError]] = None
    parse_tree: Optional[Union[ast_raw.Node, ast_raw.Leaf]] = None
    with_fqns: Union[DecodeError, ParseError, Crash, ast_cooked.Base]
    with open(args.srcpath, 'rb') as src_f:
        src_content = xcast(bytes, src_f.read())
        encoding = 'utf-8'  # TODO: get encoding from lib2to3.pgen2.tokenize.detect_encoding
        # TODO: add to ast.File: args.root, args.corpus (even though in Meta)
        try:
            src_file = ast.make_file(path=args.srcpath, content=src_content, encoding=encoding)
            parse_error = None
        except UnicodeDecodeError as exc:
            # exc.object is the entire string, which can be determined
            # from the file
            parse_error = DecodeError(encoding=exc.encoding,
                                      start=exc.start,
                                      end=exc.end,
                                      reason=exc.reason,
                                      srcpath=args.srcpath)
        if src_file:
            try:
                parse_tree = ast_raw.parse(src_content, args.python_version)
            except pgen2_tokenize.TokenError as exc:
                parse_error = ParseError(msg=str(exc),
                                         type='',
                                         context='',
                                         value='',
                                         srcpath=args.srcpath)
            except SyntaxError as exc:
                # TODO: This seems to sometimes be raised from an encoding error
                #       with msg='unknown encoding: ...', exc.lineno=None, exc.offset=None
                parse_error = ParseError(
                    msg=str(exc),
                    type='',
                    context=str(
                        ('', (exc.lineno, exc.offset))) if exc.lineno or exc.offset else '',
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

    if src_file and parse_tree:
        assert not parse_error
        logging.debug('RAW= %r', parse_tree)
        try:
            cooked_nodes = ast_raw.cvt_parse_tree(parse_tree, args.python_version, src_file)
            with_fqns = ast_cooked.add_fqns(cooked_nodes, args.module, args.python_version)
        except pgen2_parse.ParseError as exc:
            parse_error = ParseError(msg=str(exc),
                                     type=pytree.type_repr(exc.type),
                                     value=exc.value or '',
                                     context=str(exc.context),
                                     srcpath=args.srcpath)
            with_fqns = parse_error
            logging.error('Parse error: %s', parse_error)
        except Exception as exc:  # pylint: disable=broad-except
            logging.error('Caught error in cvt_parse_tree/with_fqns: %s %r', exc, exc)
            traceback.print_exc()
            with_fqns = Crash(str=str(exc), repr=repr(exc), srcpath=args.srcpath)
    else:
        logging.error('Parse error: %s', parse_error)
        assert parse_error
        with_fqns = parse_error

    # b64encode returns bytes, so use decode() to turn it into a
    # string, because json.dumps can't process bytes.
    meta = ast_cooked.Meta(
        kythe_corpus=args.kythe_corpus,
        kythe_root=args.kythe_root,
        path=args.srcpath,
        language='python',
        contents_base64=base64.b64encode(src_content).decode('ascii'),
        contents=src_content.decode(
            encoding, errors='ignore'),  # TODO: delete (for debugging only)
        sha1=hashlib.sha1(src_content).hexdigest(),
        encoding=encoding)

    with open(args.out_fqn_ast, 'w') as out_fqn_ast_file:
        logging.debug('Output fqn= %r', out_fqn_ast_file)
        print(meta.as_prolog_str() + '.', file=out_fqn_ast_file)
        print(with_fqns.as_prolog_str() + '.', file=out_fqn_ast_file)
    logging.debug('Finished')
    return 0


@dataclass(frozen=True)
class DecodeError(pod.PlainOldDataExtended):
    """Encapsulate UnicodeDecodeError."""

    encoding: Text
    start: int
    end: int
    reason: Text
    srcpath: Text
    __slots__ = ['encoding', 'start', 'end', 'reason', 'srcpath']


@dataclass(frozen=True)
class ParseError(pod.PlainOldDataExtended):
    """Encapsulate parse.ParseError."""

    msg: Text
    type: Text  # pytree.type_repr(type)
    value: Optional[Text]
    context: Text  # str(context)
    srcpath: Text
    __slots__ = ['msg', 'type', 'value', 'context', 'srcpath']


@dataclass(frozen=True)
class Crash(pod.PlainOldDataExtended):
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
