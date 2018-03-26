"""Test pykythe."""

import collections
import json
import logging  # pylint: disable=unused-import
import os
import pickle
import sys
from typing import List  # pylint: disable=unused-import
import unittest
from lib2to3 import pytree
from lib2to3.pgen2 import token

# TODO: get rid of this hack?
sys.path.insert(0,
                os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pykythe import (ast, ast_cooked, ast_raw, pod)  # pylint: disable=wrong-import-position


class SomeData(pod.PlainOldData):
    """Simple example of subclassing PlainOldData, for testing."""

    # pylint: disable=too-few-public-methods

    __slots__ = ('a', 'b', 'c')

    def __init__(self, a: int, b: int, c: int) -> None:
        super().__init__(a=a, b=b, c=c)


class SomeData2(pod.PlainOldDataExtended):
    """Simple example of subclassing PlainOldData, for testing.

    This subclass doesn't have an __init__ ... it's used for testing that
    pod.PlainOldData.__init__ verifies that all the slots are filled.
    """

    # pylint: disable=too-few-public-methods

    __slots__ = ('a', 'b', 'c')


class EmptyData(pod.PlainOldData):
    """Simple example of subclassing PlainOldData, with no contents."""


class TestPlainOldData(unittest.TestCase):
    """Unit tests for PlainOldData."""

    def test_plain_old_data(self) -> None:
        """Test that subclass of PlainOldData works as expected."""
        empty_node = EmptyData()
        self.assertTrue(empty_node)  # would fail with namedtuple
        a_node = SomeData(a=1, b=2, c=3)
        self.assertEqual(a_node, a_node)
        self.assertEqual(repr(a_node), 'SomeData(a=1, b=2, c=3)')
        self.assertEqual(a_node._asdict(),
                         collections.OrderedDict([
                             ('a', 1),
                             ('b', 2),
                             ('c', 3),
                         ]))

        a_copy = a_node._replace()
        self.assertNotEqual(id(a_node), id(a_copy))
        self.assertEqual(a_node, a_copy)

        b_node = a_node._replace(a=999)
        self.assertNotEqual(a_node, b_node)
        self.assertEqual(repr(a_node), 'SomeData(a=1, b=2, c=3)')
        self.assertEqual(a_node._asdict(),
                         collections.OrderedDict([
                             ('a', 1),
                             ('b', 2),
                             ('c', 3),
                         ]))
        self.assertEqual(repr(b_node), 'SomeData(a=999, b=2, c=3)')
        self.assertEqual(b_node._asdict(),
                         collections.OrderedDict([
                             ('a', 999),
                             ('b', 2),
                             ('c', 3),
                         ]))

        b_pickle = pickle.dumps(b_node, protocol=pickle.HIGHEST_PROTOCOL)
        b_unpickle = pickle.loads(b_pickle)
        self.assertEqual(repr(b_unpickle), 'SomeData(a=999, b=2, c=3)')
        self.assertEqual(b_unpickle._asdict(),
                         collections.OrderedDict([
                             ('a', 999),
                             ('b', 2),
                             ('c', 3),
                         ]))

        with self.assertRaises(ValueError):
            # ValueError: Unknown field names: ['x']
            a_node._replace(x=1)

        with self.assertRaises(TypeError):
            # TypeError: __init__() got an unexpected keyword argument 'x'
            SomeData(a=1, b=2, c=3, x=666)  # type: ignore  # pylint: disable=unexpected-keyword-arg

        c_1 = SomeData(a=1, b=2, c=3)
        self.assertEqual(c_1.as_json_str(), '{"a": 1, "b": 2, "c": 3}')

        with self.assertRaises(ValueError):
            # ValueError: Missing field: 'c'
            SomeData2(a=1, b=2)

        c_2 = SomeData2(a=1, b=True, c='xxx')
        # pylint: disable=line-too-long
        self.assertEqual(c_2.as_json_str(
        ), '{"kind": "SomeData2", "slots": {"a": {"kind": "int", "value": 1}, "b": {"kind": "bool", "value": "True"}, "c": {"kind": "str", "value": "xxx"}}}'
                        )

        with self.assertRaises(ValueError):
            # ValueError: Unknown field names: ['x']
            SomeData2(a=1, b=2, c=3, x=666)

        c_1a = SomeData(a=1, b=2, c=33)

        self.assertEqual(c_1, c_1)
        self.assertEqual(c_1, c_1._replace())  # test == with a copy
        self.assertNotEqual(c_1, c_2)
        self.assertNotEqual(c_1, c_1a)
        self.assertFalse(c_1 == c_1a)
        self.assertTrue(c_1 != c_1a)


class TestAnchor(unittest.TestCase):
    """Unit tests for anchors."""

    # TODO: repeat these for '\r\n' and '\n'.
    # TODO: add tests for non-ASCII.

    def test_leafs(self) -> None:
        """Simple-minded test for anchors being computed correctly."""
        # pylint: disable=too-many-locals
        content = ('# A comment\n'
                   'a, (b, x) = y = 1  # Binds `a`, `b`\n'
                   '\n'
                   'if a == 234:  # Ref `a`\n'
                   '  bcd = "<br/>"\n').encode('utf-8')
        expected_types = (token.NAME, token.NUMBER, token.STRING)
        expected = [
            b'a',
            b'b',
            b'x',
            b'y',
            b'1',
            b'if',
            b'a',
            b'234',
            b'bcd',
            b'"<br/>"',
        ]
        for python_version in 3, 2:
            parse_tree = ast_raw.parse(content, python_version)
            logging.debug('RAW= %r', parse_tree)
            src_file = ast.File(content, 'utf-8')
            self.assertEqual(content.decode('utf-8'), str(parse_tree))
            self.assertEqual(
                str(parse_tree), ''.join(
                    str(node)
                    for node in parse_tree.pre_order()
                    if isinstance(node, pytree.Leaf)))
            anchor_file = ast.File(content, 'utf-8')
            leaf_nodes = [
                node for node in parse_tree.pre_order() if
                isinstance(node, pytree.Leaf) and node.type in expected_types
            ]
            self.assertEqual(len(leaf_nodes), len(expected))
            for node, expected_str in zip(leaf_nodes, expected):
                anchor = anchor_file.astn_to_range(node)
                self.assertEqual(
                    content[anchor.start:anchor.end], expected_str)
            cooked_nodes = ast_raw.cvt_parse_tree(
                parse_tree, python_version, src_file)
            logging.debug('RAW= %r', parse_tree)
            logging.debug('COOKED= %r', cooked_nodes)
            fqn_ctx = ast_cooked.FqnCtx(
                fqn_dot='testing.',
                bindings=collections.ChainMap(),
                python_version=python_version)
            anchors = cooked_nodes.anchors(fqn_ctx)


if __name__ == '__main__':
    unittest.main()
