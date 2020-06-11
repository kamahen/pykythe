"""Generate colorization information from  lib2to3's AST."""

from dataclasses import dataclass
from lib2to3 import pytree, pygram
from lib2to3.pgen2 import token

from typing import Dict, Iterable, List, Optional

from . import ast, pod


@dataclass(frozen=True)
class Color(pod.PlainOldDataExtended):
    """Created with ColorFile._make_color."""

    astn: ast.Astn
    lineno: int
    column: int
    token_color: str

    __slots__ = ['astn', 'lineno', 'column', 'token_color']


def colored_list_as_prolog_str(colored: List[Color]) -> str:
    return '[' + ','.join(c.as_prolog_str() for c in colored) + ']'


@dataclass(frozen=True)
class ColorFile:
    """Encapsulate src_file, etc. for convenience."""

    src_file: Optional[ast.File]
    parse_tree: Optional[pytree.Base]
    # name_astns from ast_cooked.add_fqns(...).name_astns() - maps name to '<VAR_REF' etc.
    name_astns: Dict[ast.Astn, str]

    def color(self) -> List[Color]:
        """Traverse parse tree, outputting Color nodes."""
        color_list = list(self._color()) if self.src_file and self.parse_tree else []
        # DO NOT SUBMIT - remove this validation:
        assert not color_list or color_list[0].astn.start == 0, [color_list[0]]
        for i in range(1, len(color_list)):
            assert color_list[i - 1].astn.end == color_list[i].astn.start, (i, color_list[:i + 1])
        return color_list

    def _color(self) -> Iterable[Color]:
        """Generator for self.color()."""
        assert self.src_file and self.parse_tree  # For mypy
        for node in self.parse_tree.pre_order():
            if isinstance(node, pytree.Leaf):
                astn = self.src_file.node_to_astn(node)
                start_prefix = self.src_file.byte_offset_adjust_chr(astn.start, -len(node.prefix))
                yield from self._color_whitespace(node.prefix, start_prefix)
                yield from self._color_value(node, astn)
            else:
                assert isinstance(node, pytree.Node), [type(node), node]

    def _make_color(self, value: str, start: int, token_color: str) -> Iterable[Color]:
        """Yield a Color node (computing lineno, column) if value non-empty."""
        if not value:
            return
        assert self.src_file  # For mypy
        lineno, column = self.src_file.byte_offset_to_lineno_column(start)
        yield Color(astn=ast.Astn(value=value,
                                  start=start,
                                  end=self.src_file.byte_offset_adjust_chr(start, len(value))),
                    lineno=lineno,
                    column=column,
                    token_color=token_color)

    def _color_value(self, node: pytree.Leaf, astn: ast.Astn) -> Iterable[Color]:
        """Generate Color items from a leaf node's value."""
        if not node.value:
            return
        assert self.src_file and self.parse_tree  # For mypy
        if node.type == token.STRING:
            token_color = '<STRING>'
            yield from self._string_lines(astn=astn, token_color=token_color)
        else:
            if node.type == token.NAME:
                # TODO: need to handle 'print', 'exec' in Python 3 ... see
                #       ast_raw.parse.
                if astn.value in pygram.python_grammar.keywords:
                    token_color = '<KEYWORD>'
                else:
                    token_color = self.name_astns[astn]
            elif node.type == token.NUMBER:
                token_color = '<NUMBER>'
            elif node.value.isspace():
                assert node.type in {token.INDENT, token.DEDENT, token.NEWLINE, token.ENDMARKER}
                token_color = '<WHITESPACE>'
            else:
                # TODO: special case '.' when in an "import" statement
                #       e.g., if node.type == token.DOT: token_color = '<PUNCTUATION>' + token.tok_name[node.type]
                #       - this is currently handled by src_browser.pl, which looks for a
                #         semantic anchor on punctuation
                token_color = '<PUNCTUATION>'
            yield Color(astn=astn, lineno=node.lineno, column=node.column, token_color=token_color)

    def _color_whitespace(self, value: str, start: int) -> Iterable[Color]:
        """Generate Color items from whitespace/comment (prefix starting at start)."""
        if not value:  # zero-length - ignore
            return
        assert self.src_file  # For mypy
        white_lines = value.splitlines(keepends=True)
        for white_line in white_lines:
            without_newline, = white_line.splitlines(keepends=False)
            newline = white_line[len(without_newline):]
            start_newline = self.src_file.byte_offset_adjust_chr(start, len(without_newline))
            start_next = self.src_file.byte_offset_adjust_chr(start_newline, len(newline))
            yield from self._color_whitespace_line(without_newline, start)
            yield from self._make_color(newline, start_newline, '<NEWLINE>')
            start = start_next

    def _color_whitespace_line(self, value: str, start: int) -> Iterable[Color]:
        """Break up whitespace/comment into chunks and generate Color items."""
        # value guaranteed to not have an '\n' in it
        stripped_left = value.lstrip()
        before = value[:len(value) - len(stripped_left)]
        stripped = stripped_left.rstrip()
        assert self.src_file  # For mypy
        after = '' if stripped == stripped_left else stripped_left[len(stripped) -
                                                                   len(stripped_left):]
        start_at = self.src_file.byte_offset_adjust_chr(start, len(before))
        start_after = self.src_file.byte_offset_adjust_chr(start, len(before) + len(stripped))
        yield from self._make_color(before, start, '<WHITESPACE>')
        yield from self._make_color(stripped, start_at, '<COMMENT>')
        yield from self._make_color(after, start_after, '<WHITESPACE>')

    def _string_lines(self, astn: ast.Astn, token_color: str) -> Iterable[Color]:
        """Break up string into chunks and generate Color items."""
        # TODO: can this be combined with _color_whitespace?
        astn_value_pieces = astn.value.splitlines(keepends=True)
        start = astn.start
        assert self.src_file
        for piece in astn_value_pieces:
            without_newline, = piece.splitlines(keepends=False)
            newline = piece[len(without_newline):]
            start_newline = self.src_file.byte_offset_adjust_chr(start, len(without_newline))
            start_next = self.src_file.byte_offset_adjust_chr(start_newline, len(newline))
            yield from self._make_color(value=without_newline,
                                        start=start,
                                        token_color=token_color)
            yield from self._make_color(value=newline,
                                        start=start_newline,
                                        token_color=token_color)
            start = start_next
