#!/usr/bin/env python3.6
"""Main program for Python parser that outputs AST as Prolog term.

This is called by pykythe.pl, which further processes the AST.
"""

import argparse
import base64
import collections
import hashlib
import logging
import sys
from typing import List  # pylint: disable=unused-import
from .typing_debug import cast as xcast

from . import ast, ast_raw, ast_cooked


def main() -> int:
    """Main (uses sys.argv)."""
    parser = argparse.ArgumentParser(
        description='Parse Python file, generating Kythe facts')
    # TODO: allow nargs='+' for multiple inputs?
    parser.add_argument('--srcpath', required=True, help='Input file')
    parser.add_argument('--module',
                        required=True,
                        help='FQN of module corresponding to --src')
    parser.add_argument(
        '--out_fqn_ast',
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

    with open(args.srcpath, 'rb') as src_f:
        src_content = xcast(bytes, src_f.read())
        # TODO: add to ast.File: args.root, args.corpus (even though in Meta)
        src_file = ast.make_file(
            path=args.srcpath, content=src_content, encoding='utf-8'
        )  # TODO: get encoding from lib2to3.pgen2.tokenize.detect_encoding
        parse_tree = ast_raw.parse(src_content, args.python_version)

    logging.debug('RAW= %r', parse_tree)
    cooked_nodes = ast_raw.cvt_parse_tree(
        parse_tree, args.python_version, src_file)
    add_fqns = ast_cooked.add_fqns(
        cooked_nodes, args.module, args.python_version)

    # b64encode returns bytes, so use decode() to turn it into a
    # string, because json.dumps can't process bytes.
    meta = ast_cooked.Meta(
        kythe_corpus=args.kythe_corpus,
        kythe_root=args.kythe_root,
        path=args.srcpath,
        language='python',
        contents_base64=base64.b64encode(src_content).decode('ascii'),
        contents=src_content.decode(
            'utf8'),  # TODO: delete (for debugging only)
        sha1=hashlib.sha1(src_content).hexdigest(),
        encoding=src_file.encoding)

    with open(args.out_fqn_ast, 'w') as out_fqn_ast_file:
        logging.debug('Output fqn= %r', out_fqn_ast_file)
        print(meta.as_prolog_str() + '.', file=out_fqn_ast_file)
        print(add_fqns.as_prolog_str() + '.', file=out_fqn_ast_file)
    logging.debug('Finished')
    return 0


if __name__ == '__main__':
    if sys.version_info < (3, 6):
        raise RuntimeError(f'Version must be 3.6 or later: {sys.version_info}')
    sys.exit(main())
