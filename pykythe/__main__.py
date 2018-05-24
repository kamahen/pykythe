#!/usr/bin/env python3.6
"""Main program for Python parser that outputs JSON facts.

This uses lib2to3, which supports both Python2 and Python3 syntax.
"""

# TODO: The code here is temporary scaffolding, and will change
#       significantly before release.

import argparse
import base64
import collections
import json
import logging
import os.path
import sys
from typing import List  # pylint: disable=unused-import
from .typing_debug import cast as xcast

from . import ast, ast_raw, ast_cooked


def main() -> int:
    """Main (uses sys.argv)."""
    parser = argparse.ArgumentParser(
        description='Parse Python file, generating Kythe facts')
    # TODO: allow nargs='+' for multiple inputs?
    parser.add_argument('--src', required=True, help='Input file')
    parser.add_argument(
        '--out_fqn_expr',
        required=True,
        help=('output file for fqn_expr JSON facts. '
              'These are post-processed to further resolve names.'))
    parser.add_argument(
        '--corpus', default='', help='Value of "corpus" in Kythe facts')
    parser.add_argument(
        '--root', default='', help='Value of "root" in Kythe facts')
    parser.add_argument(
        '--python_version',
        default=3,
        choices=[2, 3],
        type=int,
        help='Python major version')
    args = parser.parse_args()

    with open(args.src, 'rb') as src_f:
        src_content = xcast(bytes, src_f.read())
        src_file = ast.File(
            path=args.
            src,  # TODO: add args.root, args.corpus (even though in Meta)
            content=src_content,
            encoding='utf-8')  # TODO: get encoding from parse
        parse_tree = ast_raw.parse(src_content, args.python_version)

    meta = ast_cooked.Meta(
        corpus=args.corpus,
        root=args.root,
        path=args.src,
        language='python',
        contents_b64=base64.b64encode(src_content).decode('ascii'))

    logging.debug('RAW= %r', parse_tree)
    cooked_nodes = ast_raw.cvt_parse_tree(
        parse_tree, args.python_version, src_file)
    logging.debug('COOKED= %r', cooked_nodes)
    cooked_nodes_json_dict = cooked_nodes.as_json_dict()
    logging.debug('AS_JSON_DICT= %r', cooked_nodes_json_dict)
    logging.debug('AS_JSON: %s', json.dumps(cooked_nodes_json_dict))
    fqn_ctx = ast_cooked.FqnCtx(
        fqn_dot=file_to_module(args.src) + '.',
        bindings=collections.ChainMap(collections.OrderedDict()),
        class_fqn=None,
        class_astn=None,
        python_version=args.python_version)
    add_fqns = cooked_nodes.add_fqns(fqn_ctx)

    with open(args.out_fqn_expr, 'w') as out_fqn_expr_file:
        logging.debug('Output fqn= %r', out_fqn_expr_file)
        print(meta.as_json_str(), file=out_fqn_expr_file)
        print(add_fqns.as_json_str(), file=out_fqn_expr_file)
    logging.debug('Finished')
    return 0


def file_to_module(file_name: str) -> str:
    root, _ = os.path.splitext(file_name)
    components = root.split(os.path.sep)
    return '.'.join(components)


if __name__ == '__main__':
    if sys.version_info < (3, 6):
        # Can't use f'...' because that requires 3.6:
        raise RuntimeError('Version must be 3.6 or later: {}'.format(
            sys.version_info))
    sys.exit(main())
