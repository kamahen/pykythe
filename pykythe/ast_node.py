"""AST node operations."""

import bisect
import codecs
from dataclasses import dataclass
import io
from lib2to3 import pytree
from lib2to3.pgen2 import tokenize
from typing import Any, Dict, List, Tuple
from . import pod
from .typing_debug import cast as xcast


@dataclass(frozen=True)
class Astn(pod.PlainOldDataExtended):
    """AST node - pytree.Leaf contents with byte offset.

    Start, end are in bytes, so only for ASCII is it true that
    len(self.value) == self.end - self.start.
    """

    value: str
    start: int  # in bytes (not characters)
    end: int  # in bytes (not characters)
    __slots__ = ['value', 'start', 'end']

    def as_json_dict(self) -> Dict[str, Any]:
        return dict(
                value=self.value,  # base64.b64encode(self.value.encode('utf-8')).decode('ascii'),
                start=self.start,
                end=self.end)


def make_astn(value: str, start: int) -> Astn:
    return Astn(value=value, start=start, end=start + len(value))


@dataclass(frozen=True)
class File(pod.PlainOldData):
    """Encapsulate a file for offsets, etc."""

    # pylint: disable=too-many-instance-attributes
    path: str
    contents_bytes: bytes
    contents_str: str
    encoding: str
    lineno_to_chr_offset: Dict[int, int]
    chr_to_byte_offset: Dict[int, int]
    byte_to_chr_offset: Dict[int, int]
    chr_offsets_for_linenos: List[Tuple[int, int]]
    numlines: int

    def __post_init__(self) -> None:
        # TODO: delete these validation tests
        for i in range(1, len(self.chr_offsets_for_linenos)):
            assert (self.chr_offsets_for_linenos[i - 1][0] < self.chr_offsets_for_linenos[i][0] and
                    self.chr_offsets_for_linenos[i - 1][1] < self.chr_offsets_for_linenos[i][1]), {
                            i - 1: self.chr_offsets_for_linenos[i - 1],
                            i: self.chr_offsets_for_linenos[i]}
        last_lineno = 0
        last_column = 0
        for i in range(0, len(self.contents_str)):
            lc = self.chr_offset_to_lineno_column(i)
            assert self.lineno_to_chr_offset[lc[0]] + lc[1] == i, dict(i=i, lc=lc)
            if lc == (last_lineno, last_column):
                last_column += 1
            else:
                assert lc == (last_lineno + 1, 0), dict(i=i,
                                                        lc=lc,
                                                        last_lineno=last_lineno,
                                                        last_column=last_column,
                                                        ch=self.contents_str[i])
                last_lineno += 1
                last_column = 1

    def node_to_astn(self, node: pytree.Base) -> Astn:
        """Get the Kythe anchor range from an AST leaf node."""
        node = xcast(pytree.Leaf, node)
        if self.numlines:
            offset = self.chr_to_byte_offset[self.lineno_to_chr_offset[node.lineno] + node.column]
        else:
            # Empty file is a special case:
            assert self.lineno_to_chr_offset == {1: 0}
            assert self.chr_offsets_for_linenos == [(0, 1)]
            assert self.numlines == 0
            assert node.value == '', [node]  # node == Leaf(0, '')
            offset = 0
        return make_astn(node.value, offset)

    def chr_offset_to_lineno_column(self, chr_offset: int) -> Tuple[int, int]:
        i = bisect.bisect_right(self.chr_offsets_for_linenos, (chr_offset, 9999999999))
        return (self.chr_offsets_for_linenos[i - 1][1],
                chr_offset - self.chr_offsets_for_linenos[i - 1][0])

    def byte_offset_to_lineno_column(self, byte_offset: int) -> Tuple[int, int]:
        return self.chr_offset_to_lineno_column(self.byte_to_chr_offset[byte_offset])

    def byte_offset_adjust_chr(self, byte_offset: int, chr_len: int) -> int:
        """Adjust a byte offset using a string length, giving a byte offset.

        Typical use: have an Astn (with a start position in bytes) and
        need to move forward or backward the number of bytes that are
        taken by the encoding of a string, whose len is chr_len
        (chr_len can be negative for adjusting backwards).
        """
        start_chr = self.byte_to_chr_offset[byte_offset]
        return self.chr_to_byte_offset[start_chr + chr_len]


def make_file_from_contents(path: str, contents_bytes: bytes) -> File:
    """Wrapper for File constructor.

    Computes the line offsets and creates a `File` object from
    `contents_bytes`. (`path` and `encoding` are passed through to the
    `File` object.)
    """
    # pylint: disable=too-many-locals
    with io.BytesIO(contents_bytes) as src_f:
        try:
            encoding, _ = tokenize.detect_encoding(src_f.readline)  # type: ignore
        except LookupError as exc:
            # TODO: first arg of UnicodeDecodError is encoding, but we
            #       don't know that, so this is an inappropriate error
            #       to raise.
            raise UnicodeDecodeError('???', contents_bytes, 0, 1, str(exc))
        if encoding == 'utf8-sig':  # TODO: see https://bugs.python.org/issue39155
            encoding = 'utf-8-sig'
        decoder = codecs.getincrementaldecoder(encoding)()
        chr_to_byte_offset: Dict[int, int] = {}
        chr_offset = 0
        last_byte_offset = 0
        contents_list = []
        for byte_offset, by in enumerate(contents_bytes):
            # TODO: benchmark other methods of converting an int to a byte:
            #     by.to_bytes(1, sys.byteorder, signed=False))
            #     struct.unpack('1c', by))[0]
            #     chr(by).encode('latin1')
            #   (probably these are all dwarfed by the time used
            #   to process the AST)
            ch = decoder.decode(bytes([by]))  # Can raies UnicodeDecodeError
            if ch:
                contents_list.append(ch)
                assert chr_offset not in chr_to_byte_offset
                chr_to_byte_offset[chr_offset] = last_byte_offset
                chr_offset += 1
                last_byte_offset = byte_offset + 1
        final_by = decoder.decode(b'', True)  # flush
        assert final_by == '', final_by
        contents_str = ''.join(contents_list)
        # Ast uses [star,end), so need to also have the last+1 offset:
        assert chr_offset not in chr_to_byte_offset
        chr_to_byte_offset[chr_offset] = last_byte_offset

    lineno_to_chr_offset = {1: 0}
    lineno = 1
    for offset, char in enumerate(contents_str):
        # TODO: make this work with Windows '\r\n', Mac '\r'
        #       e.g., use contents_str.splitlines(keepends=True)
        #       (see code in ast_color.ColorFile._color_whitespace).
        if char == '\n':
            lineno += 1
            lineno_to_chr_offset[lineno] = offset + 1
    byte_to_chr_offset = {v: k for k, v in chr_to_byte_offset.items()}
    assert len(byte_to_chr_offset) == len(chr_to_byte_offset)  # no dup k,v / v,k
    return File(path=path,
                contents_bytes=contents_bytes,
                contents_str=contents_str,
                encoding=encoding,
                lineno_to_chr_offset=lineno_to_chr_offset,
                chr_to_byte_offset=chr_to_byte_offset,
                byte_to_chr_offset=byte_to_chr_offset,
                chr_offsets_for_linenos=sorted((offset, lineno)
                                               for lineno, offset in lineno_to_chr_offset.items()),
                numlines=lineno - 1)


def make_file(path: str) -> File:
    """Wrapper for File constructor, given a path."""

    with open(path, 'rb') as src_f:
        contents_bytes = xcast(bytes, src_f.read())
        return make_file_from_contents(path, contents_bytes)
