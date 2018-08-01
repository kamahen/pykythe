"""AST node operations."""

import codecs
from lib2to3 import pytree
from typing import Text
from . import pod
from .typing_debug import cast as xcast


class Astn(pod.PlainOldDataExtended):
    """pytree.Leaf contents with byte offset."""

    __slots__ = ['value', 'start', 'end']

    def __init__(self, *, value: Text, start: int, end: int) -> None:
        # pylint: disable=super-init-not-called
        self.value = value
        self.start = start
        self.end = end


class File(pod.PlainOldData):
    """Encapsulate a file for offsets, etc."""

    __slots__ = ['path', 'content', 'line_offsets', 'encoding', 'numlines']

    def __init__(self, path: Text, content: bytes, encoding: Text) -> None:
        # pylint: disable=super-init-not-called
        self.path = path
        self.content = content
        self.line_offsets = {1: 0}
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

    def astn_to_range(self, astn: pytree.Base) -> Astn:
        """Get the Kythe anchor range from an AST leaf node."""
        astn = xcast(pytree.Leaf, astn)
        offset = self.line_offsets[astn.lineno] + astn.column
        return Astn(
            value=astn.value, start=offset, end=offset + len(astn.value))
