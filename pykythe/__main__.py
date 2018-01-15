#!/usr/bin/python3.6
"""Main program for Python parser that outputs JSON facts.

This uses lib2to3, which supports both Python2 and Python3 syntax.

TODO: The code here is temporary scaffolding, and will change
      significantly before release.
"""

import argparse
import collections
import logging
import os.path
import sys

from . import ast_raw
from . import ast_cooked
from . import kythe


def main() -> int:
    """Main (uses sys.argv)."""
    parser = argparse.ArgumentParser(description='Parse Python file')
    parser.add_argument('src', nargs='+', help='Input file')
    parser.add_argument(
        '--corpus', default='', help='Value of "corpus" in Kythe facts')
    parser.add_argument(
        '--root', default='', help='Value of "root" in Kythe facts')
    args = parser.parse_args()
    for src in args.src:
        with open(src, 'rb') as src_file:
            src_content = src_file.read()
            parse_tree = ast_raw.parse(src_content)
        cooked_nodes = ast_raw.cvt_tree(parse_tree)
        cooked_nodes = cooked_nodes.fqns(
            ctx=ast_cooked.FqnCtx(
                fqn=file_to_module(src), bindings=collections.ChainMap()))
        anchor_file = kythe.File(
            content=src_content,
            encoding='utf-8')  # TODO: get encoding from parse
        kythe_facts = kythe.KytheFacts(
            corpus=args.corpus,
            root=args.root,
            anchor_file=anchor_file,
            path=src,
            language='python')
        anchors = list(cooked_nodes.anchors())
        for json_fact in kythe_facts.json_facts(anchors, parse_tree):
            print(json_fact)
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
