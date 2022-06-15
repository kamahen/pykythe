"""Define PlainOldData class.

The name of this module is misleading -- its original use has been
superseded by Python 3.9's dataclasses. Instead, it acts as a kind of
mixin class for adding a few common methods for dealing with
serialization to Prolog terms.
"""

# TODO: rename this module and its classes.

# pylint: disable=too-few-public-methods

from typing import Any, Dict, List, Optional, Sequence, Union


class PlainOldData:
    """A mixin class that adds serialization methods."""

    # Subclass *must* define its own __slots__
    __slots__ = []  # type: Sequence[str]

    def as_prolog_str(self) -> str:
        """Return a string that represents a Prolog dict.

        This doesn't recursively traverse the contents of the object;
        if that is desired, you'll need to write something like
        PlainOldDataExtended.as_prolog_str.
        """
        slot_strs = []
        for slot in self.__slots__:
            value = getattr(self, slot)
            if value is not None:
                slot_strs.append(f'{_prolog_atom(slot)}:{_as_prolog_str_full(value)}')
        return 'json{' + ','.join(slot_strs) + '}'


class PlainOldDataExtended(PlainOldData):
    """PlainOlData that can serialize certain types."""

    def as_prolog_str(self) -> str:
        """Recursively turn a node into a Prolog term."""
        # TODO: refactor the common code with PlainOldData.as_prolog_str
        slot_strs = []
        for slot in self.__slots__:
            value = getattr(self, slot)
            if value is not None:
                slot_strs.append(f'{_prolog_atom(slot)}:{_as_prolog_str_full(value)}')
        return ('json{kind:' + _prolog_atom(self.__class__.__name__) + ',slots:json{' +
                ','.join(slot_strs) + '}}')


def _prolog_atom(value: str) -> str:
    """Wrap a string so that it's a valid Prolog atom."""
    # Prolog uses a slightly different convention for "\x..."
    # in strings than Python, so the best way of doing this is
    # to use the "\u...." notation.
    # TODO: This seems to slow things down quite a bit ... is there
    #       a faster way of doing it?
    #       The "if all(...)" is an attempt to short-circuit,
    #       but it doesn't seem to save much.
    # Note that 0x7f is DEL, 0x20 is blank (everything below is a
    # control character, including tab and newline).
    # if all(0x20 <= ord(ch) < 0x7f for ch in value):
    #     return "'" + value.replace('\\', '\\\\').replace("'", r"\'") + "'"
    result = ["'"]
    for ch in value:
        ord_ch = ord(ch)
        if 0x20 <= ord_ch < 0x7f:  # ASCII, not control char and not DEL?
            if ch == '\\':
                # result.extend('\\\\')
                result.extend('\\u{:04x}'.format(ord_ch))
            elif ch == "'":
                # result.extend('\\\'')
                result.extend('\\u{:04x}'.format(ord_ch))
            else:
                result.append(ch)
        else:
            result.extend('\\u{:04x}'.format(ord_ch))
    result.extend("'")
    return ''.join(result)


def _as_prolog_str_full(
        value: Optional[Union[PlainOldData, Exception, Dict[Any, Any], int, List[Any],
                              str]]) -> str:
    """Recursively turn an object into a Prolog term."""
    # pylint: disable=too-many-return-statements
    if isinstance(value, PlainOldData):
        return value.as_prolog_str()
    elif isinstance(value, list):
        return '[' + ','.join(_as_prolog_str_full(v) for v in value) + ']'
    elif isinstance(value, bool):
        return 'json{kind:bool,value:' + _prolog_atom(str(value)) + '}'
    # Originally, we had int, str return a wrapped item like bool,
    # but removing the wrapper gave 10% performance boost overall.
    elif isinstance(value, str):
        return _prolog_atom(value)
    elif isinstance(value, bytes):
        # note: [ord(i) for i in bytes(range(256)).decode('latin1')] == list(range(256))
        return _prolog_atom(value.decode('latin1'))  # shouldn't contain any char > chr(255)
    elif isinstance(value, int):
        return str(value)
    elif isinstance(value, dict):
        return ('json{kind:dict, items:json{' + ','.join(
                (_prolog_atom(key) + ':' + _as_prolog_str_full(value))
                for key, value in value.items()) + '}}')
    elif value is None:
        return "json{kind:'None'}"
    elif isinstance(value, Exception):
        return "json{kind:'Exception',value:" + _prolog_atom(repr(value)) + '}'
    else:
        raise NotImplementedError(f'{value.__class__.__name__}: Unknown value: {value!r}')
