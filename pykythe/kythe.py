"""Encapsulate Kythe facts for output.
"""

import base64
import codecs
import collections
import json
from lib2to3 import pytree
from typing import Iterator, Optional, Text

from . import pod

# pylint: disable=too-few-public-methods


class Vname(pod.PlainOldData):
    """Encapsulate a Kythe vname.

    See https://kythe.io/docs/schema/#_vname_conventions
    """

    __slots__ = ('signature', 'corpus', 'root', 'path', 'language')

    def __init__(self,
                 *,
                 signature: Optional[Text] = None,
                 corpus: Optional[Text] = None,
                 root: Optional[Text] = None,
                 path: Optional[Text] = None,
                 language: Optional[Text] = None):
        # pylint: disable=super-init-not-called
        self.signature = signature
        self.corpus = corpus
        self.root = root
        self.path = path
        self.language = language


class Anchor(pod.PlainOldData):
    """A single anchor."""

    # vname(Signature?, Corpus?, Root?, Path?, Language?)


def _add_variable(fqn, fqn_vname, kythe_facts) -> Iterator[Text]:
    """Add a single variable to kythe_facts."""
    if fqn not in kythe_facts.variables:
        kythe_facts.variables.add(fqn)
        yield json_fact(fqn_vname, 'node/kind', b'variable')


class BindingAnchor(Anchor):
    """An anchor that binds a name."""

    __slots__ = ('astn', 'fqn')

    def __init__(self, astn: pytree.Leaf, fqn: Text) -> None:
        super().__init__(astn=astn, fqn=fqn)

    def output(self, kythe_facts, anchor_vname: Vname) -> Iterator[Text]:
        fqn_vname = kythe_facts.vname(self.fqn)
        yield from _add_variable(self.fqn, fqn_vname, kythe_facts)
        yield json_edge(anchor_vname, 'defines/binding', fqn_vname)


class RefAnchor(Anchor):
    """An anchor that refers to a name."""

    __slots__ = ('astn', 'fqn')

    def __init__(self, astn: pytree.Leaf, fqn: Text) -> None:
        super().__init__(astn=astn, fqn=fqn)

    def output(self, kythe_facts, anchor_vname: Vname) -> Iterator[Text]:
        fqn_vname = kythe_facts.vname(self.fqn)
        yield from _add_variable(self.fqn, fqn_vname, kythe_facts)
        yield json_edge(anchor_vname, 'ref', kythe_facts.vname(self.fqn))


class File(pod.PlainOldData):
    """Encapsulate a file for offsets, etc."""

    __slots__ = ('content', 'line_offsets', 'encoding')

    def __init__(self, content: Text, encoding: Text):
        # pylint: disable=super-init-not-called
        self.content = content
        self.line_offsets = {}
        self.line_offsets[1] = 0
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

    def astn_to_range(self, astn: pytree.Leaf):
        """Get the Kythe anchor range from a AST leaf node."""
        offset = self.line_offsets[astn.lineno] + astn.column
        return offset, offset + len(astn.value)


class KytheFacts:
    """Encapsulates all the Kythe facts."""

    # pylint: disable=too-many-instance-attributes

    def __init__(self,
                 *,
                 anchor_file: File,
                 path: Text,
                 corpus: Optional[Text] = None,
                 root: Optional[Text] = None,
                 language: Text) -> None:
        self.anchor_file = anchor_file
        self.path = path
        self.corpus = corpus
        self.root = root
        self.language = language
        self.anchors = {}
        self.variables = set()
        self.file_vname = Vname(
            root=self.root, corpus=self.corpus, path=self.path)

    def vname(self, signature: Text):
        return Vname(
            signature=signature,
            root=self.root,
            corpus=self.corpus,
            language=self.language)

    def output_preamble(self) -> Iterator[Text]:
        """Generate file-level Kythe facts."""
        yield json_fact(self.file_vname, 'node/kind', b'file')
        yield json_fact(self.file_vname, 'text', self.anchor_file.content)

    def output(self, anchor_item: Anchor) -> Iterator[Text]:
        """Generate fact(s) for a single anchor item."""
        start, end = self.anchor_file.astn_to_range(anchor_item.astn)
        if (start, end) in self.anchors:
            anchor_vname = self.anchors[(start, end)]
        else:
            anchor_vname = Vname(
                # TODO: for debugging:  + '[' + anchor_item.astn.value + ']'
                signature='@' + str(start) + ':' + str(end),
                root=self.root,
                corpus=self.corpus,
                path=self.path)
            self.anchors[(start, end)] = anchor_vname
            yield json_fact(anchor_vname, 'node/kind', b'anchor')
            yield json_fact(anchor_vname, 'loc/start',
                            str(start).encode('ascii'))
            yield json_fact(anchor_vname, 'loc/end', str(end).encode('ascii'))
            # TODO: Remove the following when it's not needed (e.g., by http_server):
            # yield json_edge(anchor_vname, 'childof', self.file_vname)
        yield from anchor_item.output(
            kythe_facts=self, anchor_vname=anchor_vname)


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
