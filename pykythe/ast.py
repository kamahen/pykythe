"""AST node operations."""

import codecs
from dataclasses import dataclass
from lib2to3 import pytree
from typing import Dict, Text
from . import pod
from .typing_debug import cast as xcast


@dataclass(frozen=True)
class Astn(pod.PlainOldDataExtended):
    """AST node - pytree.Leaf contents with byte offset."""

    value: Text
    start: int
    end: int
    __slots__ = ['value', 'start', 'end']


@dataclass(frozen=True)
class File(pod.PlainOldData):
    """Encapsulate a file for offsets, etc."""

    path: Text
    content: bytes
    encoding: Text
    line_offsets: Dict[int, int]
    numlines: int
    __slots__ = ['path', 'content', 'line_offsets', 'encoding', 'numlines']

    def astn_to_range(self, astn: pytree.Base) -> Astn:
        """Get the Kythe anchor range from an AST leaf node."""
        astn = xcast(pytree.Leaf, astn)
        offset = self.line_offsets[astn.lineno] + astn.column
        return Astn(
            value=astn.value,
            start=offset,
            end=offset + len(astn.value),
        )


def make_file(path: Text, content: bytes, encoding: Text) -> File:
    """Wrapper for File constructor.

    Computes the line offsets and creates a `File` object from
    `content`. (`path` and `encoding` are passed through to the
    `File` object.)
    """
    line_offsets = {1: 0}
    # TODO: this only works with ASCII right now ... need to instead
    #       use a decoding iterator to get bytes that correspond to
    #       (unicode) characters
    lineno = 1
    for offset, char in enumerate(codecs.decode(content, encoding)):
        # TODO: make this work with Windows '\r\n', Mac '\r'
        if char == '\n':
            lineno += 1
            line_offsets[lineno] = offset + 1
    return File(
        path=path,
        content=content,
        encoding=encoding,
        line_offsets=line_offsets,
        numlines=lineno - 1,
    )
