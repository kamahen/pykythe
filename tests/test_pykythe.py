"""Basic unit tests for pykythe.

Most of the tests are done using the Kythe verifier. These are mainly
low-level tests that were used early in development.
"""

import collections
import dataclasses
from dataclasses import dataclass
import logging  # pylint: disable=unused-import
import os
import pickle
import sys
from typing import Any, List  # pylint: disable=unused-import
import unittest
from lib2to3 import pytree
from lib2to3.pgen2 import token

# TODO: get rid of this hack?
sys.path.insert(0,
                os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pykythe import (ast, ast_cooked, ast_raw, typing_debug, pod)  # pylint: disable=wrong-import-position


@dataclass(frozen=True)
class SomeData(pod.PlainOldData):
    """Simple example of subclassing PlainOldData, for testing."""

    a: int
    b: str
    c: bool

    __slots__ = ['a', 'b', 'c']


@dataclass(frozen=True)
class SomeData2(pod.PlainOldDataExtended):
    """Simple example of subclassing PlainOldData, for testing.

    This subclass doesn't have an __init__ ... it's used for testing that
    pod.PlainOldData.__init__ verifies that all the slots are filled.

    TODO: obsolete -- remove
    """

    a: Any
    b: Any
    c: Any

    # pylint: disable=too-few-public-methods

    __slots__ = ['a', 'b', 'c']


class EmptyData(pod.PlainOldData):
    """Simple example of subclassing PlainOldData, with no contents."""


class TestPlainOldData(unittest.TestCase):
    """Unit tests for PlainOldData."""

    def test_plain_old_data(self) -> None:
        """Test that subclass of PlainOldData works as expected."""
        empty_node = EmptyData()
        self.assertTrue(empty_node)  # would fail with namedtuple
        a_node = SomeData(a=111, b='foo', c=False)
        self.assertEqual(a_node, a_node)
        self.assertEqual(repr(a_node), "SomeData(a=111, b='foo', c=False)")

        a_copy = dataclasses.replace(a_node)
        self.assertNotEqual(id(a_node), id(a_copy))
        self.assertEqual(a_node, a_copy)

        b_node = dataclasses.replace(a_node, a=999)
        self.assertNotEqual(a_node, b_node)
        self.assertEqual(repr(a_node), "SomeData(a=111, b='foo', c=False)")
        self.assertEqual(repr(b_node), "SomeData(a=999, b='foo', c=False)")

        if False:  # TODO: this fails for b_unpickle because it's frozen
            b_pickle = pickle.dumps(b_node, protocol=pickle.HIGHEST_PROTOCOL)
            b_unpickle = pickle.loads(b_pickle)
            self.assertEqual(
                repr(b_unpickle), "SomeData(a=999, b='foo', c=True)")

        with self.assertRaises(TypeError):
            # ValueError: Unknown field names: ['x']
            dataclasses.replace(a_node, x=1)

        with self.assertRaises(TypeError):
            # TypeError: __init__() got an unexpected keyword argument 'x'
            SomeData(a=1, b='aaa', c=False, x=666)  # type: ignore  # pylint: disable=unexpected-keyword-arg

        c_1 = SomeData(a=1, b='abc', c=True)
        self.assertEqual(
            c_1.as_prolog_str(),
            "json{'a':1,'b':'abc','c':json{kind:bool,value:'True'}}")

        with self.assertRaises(TypeError):
            # ValueError: Missing field: 'c'
            SomeData2(a=1, b=2)

        c_2 = SomeData2(a=1, b='abc', c=False)
        # pylint: disable=line-too-long
        self.assertEqual(
            c_2.as_prolog_str(),
            "json{kind:'SomeData2',slots:json{'a':1,'b':'abc','c':json{kind:bool,value:'False'}}}"
        )

        with self.assertRaises(TypeError):
            # ValueError: Unknown field names: ['x']
            SomeData2(a=1, b=2, c=3, x=666)

        c_1a = SomeData(a=1, b='abc', c=False)

        self.assertEqual(c_1, c_1)
        self.assertEqual(c_1, dataclasses.replace(c_1))  # test == with a copy
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
            b'"<br/>"', ]
        for python_version in 3, 2:
            parse_tree = ast_raw.parse(content, python_version)
            logging.debug('RAW= %r', parse_tree)
            src_file = ast.make_file('<>', content, 'utf-8')
            self.assertEqual(content.decode('utf-8'), str(parse_tree))
            self.assertEqual(
                str(parse_tree), ''.join(
                    str(node)
                    for node in parse_tree.pre_order()
                    if isinstance(node, pytree.Leaf)))
            anchor_file = ast.make_file('<>', content, 'utf-8')
            leaf_nodes = [
                node for node in parse_tree.pre_order() if
                isinstance(node, pytree.Leaf) and node.type in expected_types]
            self.assertEqual(len(leaf_nodes), len(expected))
            for node, expected_str in zip(leaf_nodes, expected):
                anchor = anchor_file.astn_to_range(node)
                self.assertEqual(
                    content[anchor.start:anchor.end], expected_str)
            cooked_nodes = ast_raw.cvt_parse_tree(
                parse_tree, python_version, src_file)
            logging.debug('RAW= %r', parse_tree)
            logging.debug('COOKED= %r', cooked_nodes)
            fqn_ctx = ast_cooked.FqnCtx(fqn_dot='testing.',
                                        bindings=collections.ChainMap(),
                                        class_fqn=None,
                                        class_astn=None,
                                        python_version=python_version)
            add_fqns = cooked_nodes.add_fqns(fqn_ctx)
            self.assertEqual(
                typing_debug.cast(
                    ast_cooked.FileInput, add_fqns).scope_bindings,
                collections.OrderedDict([
                    ('a', None),
                    ('b', None),
                    ('x', None),
                    ('y', None),
                    ('bcd', None), ]))


class TestFakeSys(unittest.TestCase):
    """Unit tests for ast_cooked.FakeSys."""

    def test_fake_sys(self) -> None:
        """Test how FakeSys is used in ast_raw.py for conditionals."""

        # TODO: parameterize the version (see definition of ast_raw.FAKE_SYS)
        self.assertTrue(sys.version_info >= (3, 7))

        result = ast_raw.FAKE_SYS.eval('bool(sys.version_info >= (3, 7))')
        self.assertFalse(result.exception)
        self.assertTrue(result.result)

        result = ast_raw.FAKE_SYS.eval('bool(sys.version_info < (3, 7))')
        self.assertFalse(result.exception)
        self.assertFalse(result.result)

        result = ast_raw.FAKE_SYS.eval('bool(sysx.version_info >= (3, 7))')
        self.assertTrue(result.exception)
        self.assertIsInstance(
            result.exception,
            NameError)  # NameError("name 'sysx' is not defined")


if __name__ == '__main__':
    unittest.main()
