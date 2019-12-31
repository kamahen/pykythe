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
        return list(self._color()) if self.src_file and self.parse_tree else []

    def _color(self) -> Iterable[Color]:
        """Generator for self.color()."""
        assert self.src_file and self.parse_tree  # For mypy
        for node in self.parse_tree.pre_order():
            if isinstance(node, pytree.Leaf):
                astn = self.src_file.node_to_astn(node)
                yield from self._color_whitespace(
                        node.prefix,
                        self.src_file.byte_offset_adjust_chr(astn.start, -len(node.prefix)))
                if node.value:
                    yield from self._color_value(node, astn)
            else:
                assert isinstance(node, pytree.Node), [type(node), node]

    def _make_color(self, value: str, start: int, token_color: str) -> Color:
        """Create a Color node (computing lineno, column)."""
        assert self.src_file  # For mypy
        lineno, column = self.src_file.byte_offset_to_lineno_column(start)
        return Color(astn=ast.Astn(value=value,
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
        if node.type == token.NAME:
            # TODO: need to handle 'print', 'exec' in Python 3 ... see
            #       ast_raw.parse.
            if astn.value in pygram.python_grammar.keywords:
                token_color = '<KEYWORD>'
            else:
                token_color = self.name_astns[astn]
        elif node.type == token.NUMBER:
            token_color = '<NUMBER>'
        elif node.type == token.STRING:
            token_color = '<STRING>'
        elif node.value.isspace():
            assert node.type in {token.INDENT, token.DEDENT, token.NEWLINE, token.ENDMARKER}, [
                    node]
            token_color = '<WHITESPACE>'
        else:
            token_color = '<PUNCTUATION>'  # token.tok_name[node.type])
        yield Color(astn=astn, lineno=node.lineno, column=node.column, token_color=token_color)

    def _color_whitespace(self, value: str, start: int) -> Iterable[Color]:
        """Generate Color items from whitespace/comment (prefix starting at stat)."""
        if not value:  # zero-length - ignore
            return
        assert self.src_file  # For mypy
        white_lines = value.splitlines(keepends=True)
        for white_line in white_lines:
            without_newline, = white_line.splitlines(keepends=False)
            newline = white_line[len(without_newline):]
            yield from self._color_whitespace_line(without_newline, start)
            start = self.src_file.byte_offset_adjust_chr(start, len(without_newline))
            if newline:
                yield self._make_color(newline, start, '<NEWLINE>')
                start = self.src_file.byte_offset_adjust_chr(start, len(newline))

    def _color_whitespace_line(self, value: str, start: int) -> Iterable[Color]:
        """Break up whitespace/comment into chunks and generate Color items."""
        # value guaranteed to not have an '\n' in it
        stripped_left = value.lstrip()
        before = value[:len(value) - len(stripped_left)]
        stripped = stripped_left.rstrip()
        assert self.src_file  # For mypy
        if stripped == stripped_left:
            after = ''
        else:
            after = stripped_left[len(stripped) - len(stripped_left):]
        if before:
            yield self._make_color(
                    before,
                    self.src_file.byte_offset_adjust_chr(start, -len(before)),
                    '<WHITESPACE>',
            )
        if stripped:
            yield self._make_color(stripped, start, '<COMMENT>')
        if after:
            yield self._make_color(
                    after,
                    self.src_file.byte_offset_adjust_chr(start, len(stripped)),
                    '<WHITESPACE>',
            )
