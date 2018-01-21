"""Test pykythe."""

import collections
import json
import logging  # pylint: disable=unused-import
import os
import pickle
import sys
import unittest
from lib2to3 import pytree
from lib2to3.pgen2 import token

# TODO: get rid of this hack?
sys.path.insert(0,
                os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pykythe import ast_cooked, ast_raw, kythe, pod  # pylint: disable=wrong-import-position


class SomeData(pod.PlainOldData):
    """Simple example of subclassing PlainOldData, for testing."""

    # pylint: disable=too-few-public-methods

    __slots__ = ('a', 'b', 'c')

    def __init__(self, a: int, b: int, c: int) -> None:
        super().__init__(a=a, b=b, c=c)


class SomeData2(pod.PlainOldData):
    """Simple example of subclassing PlainOldData, for testing.

    This subclass doesn't have an __init__ ... it's used for testing that
    pod.PlainOldData.__init__ verifies that all the slots are filled.
    """

    # pylint: disable=too-few-public-methods

    __slots__ = ('a', 'b', 'c')


class TestPlainOldData(unittest.TestCase):
    """Unit tests for PlainOldData."""

    def test_plain_old_data(self):
        """Test that subclass of PlainOldData works as expected."""
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
            SomeData(a=1, b=2, c=3, x=666)  # pylint: disable=unexpected-keyword-arg

        with self.assertRaises(ValueError):
            # ValueError: Missing field: 'c'
            SomeData2(a=1, b=2)

        SomeData2(a=1, b=2, c=3)

        with self.assertRaises(ValueError):
            # ValueError: Unknown field names: ['x']
            SomeData2(a=1, b=2, c=3, x=666)


class TestAnchor(unittest.TestCase):
    """Unit tests for anchors."""

    # TODO: repeat these for '\r\n' and '\n'.
    # TODO: add tests for non-ASCII.

    def test_leafs(self):
        """Simple-minded test for anchors being computed correctly."""
        content = ('# A comment\n'
                   'a = 1  # Binds `a`\n'
                   '\n'
                   'if a == 234:  # Ref `a`\n'
                   '  bcd = "<br/>"\n').encode('utf-8')
        expected_types = (token.NAME, token.NUMBER, token.STRING)
        expected = [
            b'a',
            b'1',
            b'if',
            b'a',
            b'234',
            b'bcd',
            b'"<br/>"',
        ]
        parse_tree = ast_raw.parse(content)

        self.assertEqual(content.decode('utf-8'), str(parse_tree))
        self.assertEqual(
            str(parse_tree), ''.join(
                str(node)
                for node in parse_tree.pre_order()
                if isinstance(node, pytree.Leaf)))
        anchor_file = kythe.File(content, 'utf-8')
        leaf_nodes = [
            node for node in parse_tree.pre_order()
            if isinstance(node, pytree.Leaf) and node.type in expected_types
        ]
        self.assertEqual(len(leaf_nodes), len(expected))
        for node, expected_str in zip(leaf_nodes, expected):
            start, end = anchor_file.astn_to_range(node)
            self.assertEqual(content[start:end], expected_str)
        cooked_nodes = ast_raw.cvt_tree(parse_tree)
        cooked_nodes = cooked_nodes.fqns(
            ctx=ast_cooked.FqnCtx(
                fqn='testing', bindings=collections.ChainMap()))
        anchors = list(cooked_nodes.anchors())
        self.assertEqual(anchors, [
            kythe.BindingAnchor(
                astn=pytree.Leaf(token.NAME, 'a'), fqn='testing.a'),
            kythe.RefAnchor(
                astn=pytree.Leaf(token.NAME, 'a'), fqn='testing.a'),
            kythe.BindingAnchor(
                astn=pytree.Leaf(token.NAME, 'bcd'), fqn='testing.bcd')
        ])
        self.assertEqual(
            [(kythe._prefix_to_html(anchor.astn.prefix),  # pylint: disable=protected-access
              anchor.value_to_html(anchor_file)) for anchor in anchors],
            [
                ('<span class="comment">#&nbsp;A&nbsp;comment<br/></span>',
                 '<span class="bind" id="@12-13"><a href="xref?q=%4012-13">a</a></span>'
                ),
                ('&nbsp;',
                 '<span class="ref" id="@35-36"><a href="xref?q=%4035-36">a</a></span>'
                ),
                ('',
                 '<span class="bind" id="@58-61"><a href="xref?q=%4058-61">bcd</a></span>'
                ),
            ],
        )
        self.maxDiff = None  # pylint: disable=invalid-name
        # pylint: disable=line-too-long
        self.assertEqual(
            kythe.html_lines(parse_tree, anchors, kythe.File(content,
                                                             'utf-8')),
            '<br/>\n'.join([
                '<span class="comment">#&nbsp;A&nbsp;comment</span>',
                ('<span class="bind" id="@12-13"><a href="xref?q=%4012-13">a</a></span>'
                 '&nbsp;=&nbsp;1<span class="comment">'
                 '&nbsp;&nbsp;#&nbsp;Binds&nbsp;`a`</span>'),
                '',
                ('<span class="reserved">if</span>&nbsp;'
                 '<span class="ref" id="@35-36"><a href="xref?q=%4035-36">a</a></span>'
                 '&nbsp;==&nbsp;234<span class="punc">:</span>'
                 '<span class="comment">&nbsp;&nbsp;#&nbsp;Ref&nbsp;`a`</span>'
                ),
                ('&nbsp;&nbsp;<span class="bind" id="@58-61"><a href="xref?q=%4058-61">bcd</a></span>'
                 '&nbsp;=&nbsp;<span class="string">&quot;&lt;br/&gt;&quot;</span>'
                ),
                '',
            ]))


class TestJson(unittest.TestCase):
    """Unit tests for emitting kythe facts as Json."""

    def test_json_1(self):
        """
        Example from
        https://kythe.io/docs/schema/writing-an-indexer.html#_bootstrapping_kythe_support
        """

        node_vname = kythe.Vname(corpus='example', path='hello')

        self.assertEqual(
            json.loads(kythe.json_fact(node_vname, 'node/kind', b'file')),
            {
                "source": {
                    "corpus": "example",
                    "path": "hello"
                },
                "fact_name": "/kythe/node/kind",
                "fact_value": "ZmlsZQ=="
            },
        )
        self.assertEqual(
            json.loads(kythe.json_fact(node_vname, 'text', b'Hello, world!')),
            {
                "source": {
                    "corpus": "example",
                    "path": "hello"
                },
                "fact_name": "/kythe/text",
                "fact_value": "SGVsbG8sIHdvcmxkIQ=="
            },
        )

    def test_json_2(self):
        """
        Example from
        https://kythe.io/docs/schema/writing-an-indexer.html#_specifying_spans_of_text
        """

        def make_anchor_vname(file_vname, begin, end):
            return kythe.Vname(
                signature='@' + str(begin) + ':' + str(end),
                path=file_vname.path,
                root=file_vname.root,
                corpus=file_vname.corpus,
                language='ex')

        begin, end = 4, 7
        file_vname = kythe.Vname(corpus='example', path='hello')
        anchor_vname = make_anchor_vname(
            file_vname=file_vname, begin=begin, end=end)
        self.assertEqual(
            json.loads(kythe.json_fact(anchor_vname, 'node/kind', b'anchor')),
            {
                "source": {
                    "signature": "@4:7",
                    "path": "hello",
                    "language": "ex",
                    "corpus": "example",
                },
                "fact_name": "/kythe/node/kind",
                "fact_value": "YW5jaG9y",
            },
        )
        self.assertEqual(
            json.loads(
                kythe.json_fact(anchor_vname, 'loc/start',
                                str(begin).encode('ascii'))),
            {
                "source": {
                    "signature": "@4:7",
                    "path": "hello",
                    "language": "ex",
                    "corpus": "example",
                },
                "fact_name": "/kythe/loc/start",
                "fact_value": "NA==",
            },
        )
        self.assertEqual(
            json.loads(
                kythe.json_fact(anchor_vname, 'loc/end',
                                str(end).encode('ascii'))),
            {
                "source": {
                    "signature": "@4:7",
                    "path": "hello",
                    "language": "ex",
                    "corpus": "example",
                },
                "fact_name": "/kythe/loc/end",
                "fact_value": "Nw==",
            },
        )
        self.assertEqual(
            json.loads(kythe.json_edge(anchor_vname, 'childof', file_vname)),
            {
                "source": {
                    "signature": "@4:7",
                    "path": "hello",
                    "language": "ex",
                    "corpus": "example",
                },
                "edge_kind": "/kythe/edge/childof",
                "target": {
                    "path": "hello",
                    "corpus": "example",
                },
                "fact_name": "/",
            },
        )


if __name__ == '__main__':
    unittest.main()
