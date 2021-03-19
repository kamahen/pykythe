"""Basic unit tests for pykythe.

Most of the tests are done using the Kythe verifier. These are mainly
low-level tests that were used early in development.
"""

import collections
import dataclasses
from dataclasses import dataclass
import logging
import os
import pickle
import sys
from typing import Any
import unittest
from lib2to3 import pytree
from lib2to3.pgen2 import token

# TODO: get rid of this hack?
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pykythe import (ast, ast_cooked, ast_raw, fakesys, typing_debug, pod, ast_color)  # pylint: disable=wrong-import-position


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


class EmptyData(pod.PlainOldData):  # pylint: disable=too-few-public-methods
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

        # TODO: this fails for b_unpickle because it's frozen
        if False:  # pylint: disable=using-constant-test
            b_pickle = pickle.dumps(b_node, protocol=pickle.HIGHEST_PROTOCOL)
            b_unpickle = pickle.loads(b_pickle)
            self.assertEqual(repr(b_unpickle), "SomeData(a=999, b='foo', c=True)")

        with self.assertRaises(TypeError):
            # ValueError: Unknown field names: ['x']
            dataclasses.replace(a_node, x=1)

        with self.assertRaises(TypeError):
            # TypeError: __init__() got an unexpected keyword argument 'x'
            SomeData(a=1, b='aaa', c=False, x=666)  # type: ignore  # pylint: disable=unexpected-keyword-arg

        c_1 = SomeData(a=1, b='abc', c=True)
        self.assertEqual(
                c_1.as_prolog_str(), "json{'a':1,'b':'abc','c':json{kind:bool,value:'True'}}")

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
        src_bytes = ('# A comment\n'
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
        for python_version in (3, ):  # Might change in future: fakesys.FAKE_SYS.version_info
            src_file = ast.make_file_from_contents('<>', src_bytes)
            parse_tree = ast_raw.parse(src_file, python_version)
            logging.debug('RAW= %r', parse_tree)
            self.assertEqual(src_bytes.decode('utf-8'), str(parse_tree))
            self.assertEqual(
                    str(parse_tree), ''.join(
                            str(node)
                            for node in parse_tree.pre_order()
                            if isinstance(node, pytree.Leaf)))
            anchor_file = ast.make_file_from_contents('<>', src_bytes)
            leaf_nodes = [
                    node for node in parse_tree.pre_order()
                    if isinstance(node, pytree.Leaf) and node.type in expected_types]
            self.assertEqual(len(leaf_nodes), len(expected))
            for node, expected_str in zip(leaf_nodes, expected):
                anchor = anchor_file.node_to_astn(node)
                self.assertEqual(src_bytes[anchor.start:anchor.end], expected_str)
            cooked_nodes = ast_raw.cvt_parse_tree(parse_tree, python_version, src_file)
            logging.debug('RAW= %r', parse_tree)
            logging.debug('COOKED= %r', cooked_nodes)
            fqn_ctx = ast_cooked.FqnCtx(fqn='testing',
                                        parent_fqn='testing',
                                        fqn_stack=[],
                                        bindings=collections.ChainMap(),
                                        class_fqn=None,
                                        class_astn=None,
                                        python_version=python_version)
            add_fqns = cooked_nodes.add_fqns(fqn_ctx)
            self.assertEqual(
                    typing_debug.cast(ast_cooked.FileInput, add_fqns).scope_bindings,
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

        result = fakesys.FAKE_SYS.eval('bool(sys.version_info >= (3, 7))')
        self.assertFalse(result.exception)
        self.assertTrue(result.result)

        result = fakesys.FAKE_SYS.eval('bool(sys.version_info < (3, 7))')
        self.assertFalse(result.exception)
        self.assertFalse(result.result)

        result = fakesys.FAKE_SYS.eval('bool(sysx.version_info >= (3, 7))')
        self.assertTrue(result.exception)
        self.assertIsInstance(
                result.exception, NameError)  # NameError("name 'sysx' is not defined")


class TestColor(unittest.TestCase):
    """Unit tests for ast_color."""

    def test_color(self) -> None:
        src_bytes = (
                '# A comment\n'  # Line 1
                'a, (b, x) = y = 1  # Binds `a`, `b`\n'  # Line 2
                '# Comment 2\n'  # Line 3
                '\n'  # Line 4
                '# Comment 3\n'  # Line 5
                'if a == 234:  # Ref `a`\n'  # Line 6
                '  # Comment 4\n'  # Line 7
                '  bcd = "<br/>"\n').encode('utf-8')  # Line 8
        src_file = ast.make_file_from_contents(path='<string>', contents_bytes=src_bytes)
        for python_version in (3, ):  # Might change in future: fakesys.FAKE_SYS.version_info
            parse_tree = ast_raw.parse(src_file, python_version)
            for node in parse_tree.pre_order():
                if isinstance(node, pytree.Leaf):
                    astn = src_file.node_to_astn(node)
                    self.assertEqual((node.lineno, node.column),
                                     src_file.byte_offset_to_lineno_column(astn.start))
            # DO NOT SUBMIT: uncomment following (and provide make_astn)
            continue
            colored = list(ast_color.color(parse_tree, src_file, {}))
            self.assertEqual(0, colored[0].astn.start)
            last_astn = colored[0].astn
            for c in colored[1:]:
                self.assertEqual(c.astn.start, last_astn.end)
                self.assertEqual(
                        c.astn,
                        src_file.node_to_astn(
                                pytree.Leaf(type=0,
                                            value=c.astn.value,
                                            context=('', (c.lineno, c.column)))))
                last_astn = c.astn
            self.assertEqual(
                    colored[0].as_json_str(),
                    '{"token_type": "<COMMENT>", '
                    '"lineno": 1, '
                    '"column": 0, '
                    '"astn": {"value": "' + '# A comment'
                    +  # base64.b64encode('# A comment\n'.encode('utf-8')).decode('ascii') +
                    '", "start": 0, "end": 11}}',
            )
            self.assertEqual(
                    colored[0].as_prolog_str(), "json{kind:'Color',"
                    "slots:json{'"
                    "astn':json{kind:'Astn',slots:json{'value':'" + "# A comment" +
                    "','start':0,'end':11}},"
                    "'lineno':1,'column':0,'token_type':'<COMMENT>'}}")
            ast_color.colored_list_as_prolog_str(colored)  # ensure it doesn't crash
            self.assertEqual(colored, [
                    ast_color.Color(astn=ast.Astn(value='# A comment', start=0, end=11),
                                    lineno=1,
                                    column=0,
                                    token_type='<COMMENT>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=11, end=12),
                                    lineno=1,
                                    column=11,
                                    token_type='<NEWLINE>'),
                    ast_color.Color(astn=ast.Astn(value='a', start=12, end=13),
                                    lineno=2,
                                    column=0,
                                    token_type='<NAME>'),
                    ast_color.Color(astn=ast.Astn(value=',', start=13, end=14),
                                    lineno=2,
                                    column=1,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=14, end=15),
                                    lineno=2,
                                    column=2,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='(', start=15, end=16),
                                    lineno=2,
                                    column=3,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value='b', start=16, end=17),
                                    lineno=2,
                                    column=4,
                                    token_type='<NAME>'),
                    ast_color.Color(astn=ast.Astn(value=',', start=17, end=18),
                                    lineno=2,
                                    column=5,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=18, end=19),
                                    lineno=2,
                                    column=6,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='x', start=19, end=20),
                                    lineno=2,
                                    column=7,
                                    token_type='<NAME>'),
                    ast_color.Color(astn=ast.Astn(value=')', start=20, end=21),
                                    lineno=2,
                                    column=8,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=21, end=22),
                                    lineno=2,
                                    column=9,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='=', start=22, end=23),
                                    lineno=2,
                                    column=10,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=23, end=24),
                                    lineno=2,
                                    column=11,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='y', start=24, end=25),
                                    lineno=2,
                                    column=12,
                                    token_type='<NAME>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=25, end=26),
                                    lineno=2,
                                    column=13,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='=', start=26, end=27),
                                    lineno=2,
                                    column=14,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=27, end=28),
                                    lineno=2,
                                    column=15,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='1', start=28, end=29),
                                    lineno=2,
                                    column=16,
                                    token_type='<NUMBER>'),
                    ast_color.Color(astn=ast.Astn(value='  ', start=29, end=31),
                                    lineno=2,
                                    column=17,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='# Binds `a`, `b`', start=31, end=47),
                                    lineno=2,
                                    column=19,
                                    token_type='<COMMENT>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=47, end=48),
                                    lineno=2,
                                    column=35,
                                    token_type='<NEWLINE>'),
                    ast_color.Color(astn=ast.Astn(value='# Comment 2', start=48, end=59),
                                    lineno=3,
                                    column=0,
                                    token_type='<COMMENT>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=59, end=60),
                                    lineno=3,
                                    column=11,
                                    token_type='<NEWLINE>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=60, end=61),
                                    lineno=4,
                                    column=0,
                                    token_type='<NEWLINE>'),
                    ast_color.Color(astn=ast.Astn(value='# Comment 3', start=61, end=72),
                                    lineno=5,
                                    column=0,
                                    token_type='<COMMENT>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=72, end=73),
                                    lineno=5,
                                    column=11,
                                    token_type='<NEWLINE>'),
                    ast_color.Color(astn=ast.Astn(value='if', start=73, end=75),
                                    lineno=6,
                                    column=0,
                                    token_type='<KEYWORD>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=75, end=76),
                                    lineno=6,
                                    column=2,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='a', start=76, end=77),
                                    lineno=6,
                                    column=3,
                                    token_type='<NAME>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=77, end=78),
                                    lineno=6,
                                    column=4,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='==', start=78, end=80),
                                    lineno=6,
                                    column=5,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=80, end=81),
                                    lineno=6,
                                    column=7,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='234', start=81, end=84),
                                    lineno=6,
                                    column=8,
                                    token_type='<NUMBER>'),
                    ast_color.Color(astn=ast.Astn(value=':', start=84, end=85),
                                    lineno=6,
                                    column=11,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value='  ', start=85, end=87),
                                    lineno=6,
                                    column=12,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='# Ref `a`', start=87, end=96),
                                    lineno=6,
                                    column=14,
                                    token_type='<COMMENT>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=96, end=97),
                                    lineno=6,
                                    column=23,
                                    token_type='<NEWLINE>'),
                    ast_color.Color(astn=ast.Astn(value='  ', start=97, end=99),
                                    lineno=7,
                                    column=0,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='# Comment 4', start=99, end=110),
                                    lineno=7,
                                    column=2,
                                    token_type='<COMMENT>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=110, end=111),
                                    lineno=7,
                                    column=13,
                                    token_type='<NEWLINE>'),
                    ast_color.Color(astn=ast.Astn(value='  ', start=111, end=113),
                                    lineno=8,
                                    column=0,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='bcd', start=113, end=116),
                                    lineno=8,
                                    column=2,
                                    token_type='<NAME>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=116, end=117),
                                    lineno=8,
                                    column=5,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='=', start=117, end=118),
                                    lineno=8,
                                    column=6,
                                    token_type='<PUNCTUATION>'),
                    ast_color.Color(astn=ast.Astn(value=' ', start=118, end=119),
                                    lineno=8,
                                    column=7,
                                    token_type='<WHITESPACE>'),
                    ast_color.Color(astn=ast.Astn(value='"<br/>"', start=119, end=126),
                                    lineno=8,
                                    column=8,
                                    token_type='<STRING>'),
                    ast_color.Color(astn=ast.Astn(value='\n', start=126, end=127),
                                    lineno=8,
                                    column=15,
                                    token_type='<NEWLINE>'), ])


if __name__ == '__main__':
    unittest.main()
