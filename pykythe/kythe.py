"""Encapsulate Kythe facts for output.
"""

import base64
import codecs
import collections
import html
import json
import logging
from lib2to3 import pytree
from lib2to3.pgen2 import token
from typing import Dict, Iterator, List, Optional, Text, Tuple, Sequence, Set  # pylint: disable=unused-import

from . import pod

# pylint: disable=too-few-public-methods


class Vname(pod.PlainOldData):
    """Encapsulate a Kythe vname.

    See https://kythe.io/docs/schema/#_vname_conventions
    """

    __slots__ = ('signature', 'corpus', 'root', 'path', 'language')

    def __init__(self,
                 *,
                 signature: Text = None,
                 corpus: Text = None,
                 root: Text = None,
                 path: Text = None,
                 language: Text = None) -> None:
        # pylint: disable=super-init-not-called
        self.signature = signature
        self.corpus = corpus
        self.root = root
        self.path = path
        self.language = language


class File(pod.PlainOldData):
    """Encapsulate a file for offsets, etc."""

    __slots__ = ('content', 'line_offsets', 'encoding', 'numlines')

    def __init__(self, content: bytes, encoding: Text) -> None:
        # pylint: disable=super-init-not-called
        self.content = content
        self.line_offsets = {1: 0}  # type: Dict[int, int]
        self.encoding = encoding
        # TODO: this only works with ASCII right now ... need to instead
        #       use a decoding iterator to get bytes that correspond to
        #       (unicode) characters
        lineno = 1
        for offset, char in enumerate(codecs.decode(content, encoding)):
            # TODO: make this work with Windows '\r\n', Mac '\r'
            if char == '\n':
                lineno += 1
                self.line_offsets[lineno] = offset + 1
        self.numlines = lineno - 1

    def astn_to_range(self, astn: pytree.Leaf):
        """Get the Kythe anchor range from a AST leaf node."""
        offset = self.line_offsets[astn.lineno] + astn.column
        return offset, offset + len(astn.value)


class KytheFacts:
    """Encapsulates all the Kythe facts.

    Attributes:
      anchor_file: The file from which anchors are derived
      path: (vname) path to the file
      corpus: vname's corpus
      root: vname's corpus
      language: vname's language
      _anchors: mapping of (start, end) to anchor vname (to prevent duplicates)
      _fqns: set of FQNs that have been defined (to prevent duplicates)
      file_vname: vname of `anchor_file`
    """

    # pylint: disable=too-many-instance-attributes

    def __init__(self,
                 *,
                 anchor_file: File,
                 path: Text,
                 corpus: Text = None,
                 root: Text = None,
                 language: Text) -> None:
        self.anchor_file = anchor_file
        self.path = path
        self.corpus = corpus
        self.root = root
        self.language = language
        self._anchors = {}  # type: Dict[Tuple[int, int], Vname]
        self._fqns = set()  # type: Set[Text]
        self.file_vname = Vname(
            root=self.root, corpus=self.corpus, path=self.path)

    def vname(self, signature: Text) -> Vname:
        return Vname(
            signature=signature,
            root=self.root,
            corpus=self.corpus,
            language=self.language)

    def json_facts(self,
                   anchors: Sequence['Anchor'],
                   parse_tree: pytree.Base = None) -> Iterator[Text]:
        """Generate file-level Kythe facts + all anchor facts."""
        yield json_fact(self.file_vname, 'node/kind', b'file')
        yield json_fact(self.file_vname, 'text', self.anchor_file.content)
        yield json_fact(self.file_vname, 'x-numlines',
                        str(self.anchor_file.numlines).encode('utf-8'))
        if parse_tree:
            yield json_fact(self.file_vname, 'x-html',
                            html_lines(parse_tree, anchors,
                                       self.anchor_file).encode('utf-8'))
        for anchor_item in anchors:
            yield from self.json_facts_for_anchor(anchor_item)

    def json_facts_for_anchor(self, anchor_item: 'Anchor') -> Iterator[Text]:
        """Generate Kythe fact(s) for a single anchor item."""
        start, end = self.anchor_file.astn_to_range(anchor_item.astn)
        if (start, end) in self._anchors:
            anchor_vname = self._anchors[(start, end)]
        else:
            anchor_vname = Vname(
                # TODO: for debugging:  + '[' + anchor_item.astn.value + ']'
                signature='@' + str(start) + ':' + str(end),
                root=self.root,
                corpus=self.corpus,
                path=self.path)
            self._anchors[(start, end)] = anchor_vname
            yield json_fact(anchor_vname, 'node/kind', b'anchor')
            yield json_fact(anchor_vname, 'loc/start',
                            str(start).encode('ascii'))
            yield json_fact(anchor_vname, 'loc/end', str(end).encode('ascii'))
            # TODO: Remove the following when it's not needed (e.g., by http_server):
            # yield json_edge(anchor_vname, 'childof', self.file_vname)
        yield from anchor_item.facts(
            kythe_facts=self, anchor_vname=anchor_vname)

    def add_variable(self, fqn, fqn_vname, kind: bytes,
                     subkind: bytes = None) -> Iterator[Text]:
        """Add a single variable to self."""
        if fqn not in self._fqns:
            self._fqns.add(fqn)
            if kind:
                yield json_fact(fqn_vname, 'node/kind', kind)
            if subkind:
                yield json_fact(fqn_vname, 'subkind', subkind)


# The following functions are based on the javascript code in
# https://kythe.io/docs/schema/writing-an-indexer.html#_modeling_kythe_entries

# The data layout is specified by message Entry in kythe/kythe/proto/storage.proto
#    Vname source
#    string edge_kind
#    VName target
#    string fact_name
#    bytes fact_value

# OrderedDict instead of dict is used to ensure that the JSON output
# is deterministic.


def json_fact(node: Vname, fact_name: str, fact_value: bytes) -> Text:
    out = collections.OrderedDict([
        ('source', node.as_json_dict()),
        ('fact_name', '/kythe/' + fact_name),
        ('fact_value', base64.b64encode(fact_value).decode('ascii')),
    ])
    return json.dumps(out)


def json_edge(source: Vname, edge_name: str, target: Vname) -> Text:
    out = collections.OrderedDict([
        ('source', source.as_json_dict()),
        ('edge_kind', '/kythe/edge/' + edge_name),
        ('target', target.as_json_dict()),
        ('fact_name', '/'),
    ])
    return json.dumps(out)


def json_ordinal_edge(source: Vname, edge_name: str, ordinal: int,
                      target: Vname) -> Text:
    return json_edge(source, edge_name + '.' + str(ordinal), target)


class Anchor(pod.PlainOldData):
    """A single anchor."""

    __slots__ = ('astn', 'fqn')

    # html_class is used to HTML <span class="...">
    # Each subclass must define this.
    html_class = None  # type: Optional[Text]

    def __init__(self, astn: pytree.Leaf, fqn: Text) -> None:
        # pylint: disable=super-init-not-called
        self.astn = astn
        self.fqn = fqn
        assert astn.type == token.NAME, [self]
        assert not any(c.isspace() for c in astn.value), [self]

    def facts(self, kythe_facts: KytheFacts,
              anchor_vname: Vname) -> Iterator[Text]:
        """Generate Kythe fact(s) for a single anchor item."""
        raise NotImplementedError(self)

    def value_to_html(self, anchor_file: File) -> Text:
        """Generate HTML for this anchor."""
        assert self.html_class
        start, end = anchor_file.astn_to_range(self.astn)
        return '<span class="{}" id="@{:d}-{:d}">{}</span>'.format(
            self.html_class, start, end, _expand_to_html(self.astn.value))


class BindingAnchor(Anchor):
    """An anchor that binds a name."""

    html_class = 'bind'

    def facts(self, kythe_facts: KytheFacts,
              anchor_vname: Vname) -> Iterator[Text]:
        fqn_vname = kythe_facts.vname(self.fqn)
        yield from kythe_facts.add_variable(self.fqn, fqn_vname, b'variable')
        yield json_edge(anchor_vname, 'defines/binding', fqn_vname)


class ClassDefAnchor(Anchor):
    """An anchor that defines a class."""

    html_class = 'class'

    # TODO: add bases

    def facts(self, kythe_facts: KytheFacts,
              anchor_vname: Vname) -> Iterator[Text]:
        fqn_vname = kythe_facts.vname(self.fqn)
        yield from kythe_facts.add_variable(self.fqn, fqn_vname, b'record',
                                            b'class')
        yield json_edge(anchor_vname, 'defines/binding', fqn_vname)


class FuncDefAnchor(Anchor):
    """An anchor that defines a function."""

    html_class = 'func'

    def facts(self, kythe_facts: KytheFacts,
              anchor_vname: Vname) -> Iterator[Text]:
        fqn_vname = kythe_facts.vname(self.fqn)
        yield from kythe_facts.add_variable(self.fqn, fqn_vname, b'function')
        yield json_edge(anchor_vname, 'defines/binding', fqn_vname)


class RefAnchor(Anchor):
    """An anchor that refers to a name."""

    html_class = 'ref'

    def facts(self, kythe_facts: KytheFacts,
              anchor_vname: Vname) -> Iterator[Text]:
        fqn_vname = kythe_facts.vname(self.fqn)
        yield from kythe_facts.add_variable(self.fqn, fqn_vname, b'variable')
        yield json_edge(anchor_vname, 'ref', kythe_facts.vname(self.fqn))


def html_lines(parse_tree: pytree.Base, anchors: Sequence[Anchor],
               anchor_file: File) -> Text:
    """Generate HTML for the file, into list of lines."""
    # TODO: The dict of (lineno, column) isn't needed if we "zip"
    #       the anchors and leaf nodes. But the hash code is
    #       easier to write. ;)
    # TODO: This code assumes that there's only anchor per node;
    #       but this won't be true when `import` is full
    #       implemented (because it both defines a binding and
    #       references an imported module).
    anchor_astns = {(anchor.astn.lineno, anchor.astn.column): anchor
                    for anchor in anchors}
    # We use the fact that joining the `str` of all Leaf nodes
    # regenerates the source.
    return ''.join(
        _astn_to_html(node, anchor_astns, anchor_file)
        for node in parse_tree.pre_order()
        if isinstance(node, pytree.Leaf))


def _astn_to_html(node: pytree.Leaf,
                  anchor_astns: Dict[Tuple[int, int], Anchor],
                  anchor_file: File) -> Text:
    anchor_astn = anchor_astns.get((node.lineno, node.column))
    if anchor_astn:
        result = anchor_astn.value_to_html(anchor_file)
    elif node.type in (token.COLON, token.COMMA):
        result = _html_span('punc', node.value)
    elif node.type == token.NAME:  # identifier already processed as anchor
        result = _html_span('reserved', node.value)
    elif node.type == token.NUMBER:
        result = _expand_to_html(node.value)
    elif node.type == token.STRING:
        result = _html_span('string', node.value)
    elif token.ISTERMINAL(node.type):
        result = _html_span('reserved', node.value)
        result = _expand_to_html(node.value)
    result = _prefix_to_html(node.prefix) + result
    return result.replace('<br/></span>', '</span><br/>')


def _prefix_to_html(prefix: Text) -> Text:
    if not prefix:
        return prefix
    if prefix.isspace():
        return _expand_to_html(prefix)
    return _html_span('comment', prefix)


def _html_span(html_class: Text, text: Text) -> Text:
    return '<span class="{}">{}</span>'.format(html_class,
                                               _expand_to_html(text))


def _expand_to_html(text: Text) -> Text:
    # splitlines throws away a final '\n', so we add a character to
    # the end, then ignore that character
    text2 = '<br/>'.join(
        html.escape(line.expandtabs())
        for line in (text + '-').splitlines())[:-1]
    # TODO: properly handle Unicode whitespace:
    return ''.join('&nbsp;' if c.isspace() else c for c in text2)
