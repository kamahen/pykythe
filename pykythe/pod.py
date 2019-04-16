"""Define PlainOldData class.

The name of this module is misleading -- its original use has been
superseded by Python 3.7's dataclasses. Instead, it acts as a kind of
mixin class for adding a few common methods for dealing with
serialization to Prolog terms.
"""

# TODO: rename this module and its classes.

# pylint: disable=too-few-public-methods

from typing import Any, Sequence, Text


class PlainOldData:
    """A mixin class that adds serialization methods."""

    # Subclass *must* define its own __slots__
    __slots__ = []  # type: Sequence[Text]

    def as_prolog_str(self) -> Text:
        """Return a string that represents a Prolog dict.

        This doesn't recursively traverse the contents of the object;
        if that is desired, you'll need to write something like
        PlainOldDataExtended.as_prolog_str.
        """
        attr_strs = []
        for attr in self.__slots__:
            value = getattr(self, attr)
            if value is not None:
                attr_strs.append(f'{_prolog_atom(attr)}:{_as_prolog_str_full(value)}')
        return 'json{' + ','.join(attr_strs) + '}'


class PlainOldDataExtended(PlainOldData):
    """PlainOlData that can serialize certain types."""

    def as_prolog_str(self) -> Text:
        """Recursively turn a node into a Prolog term."""
        attr_strs = []
        for slot in self.__slots__:
            value = getattr(self, slot)
            if value is not None:
                attr_strs.append(f'{_prolog_atom(slot)}:{_as_prolog_str_full(value)}')
        return ('json{kind:' + _prolog_atom(self.__class__.__name__) + ',slots:json{' +
                ','.join(attr_strs) + '}}')


def _prolog_atom(value: Text) -> Text:
    """Wrap a string so that it's a valid Prolog atom."""
    return "'" + value.replace('\\', '\\\\').replace("'", r"\'") + "'"


def _as_prolog_str_full(value: Any) -> Text:
    """Recursively turn an object into a Prolog term."""
    # pylint: disable=too-many-return-statements
    if isinstance(value, PlainOldData):
        return value.as_prolog_str()
    if isinstance(value, list):
        return '[' + ','.join(_as_prolog_str_full(v) for v in value) + ']'
    if isinstance(value, bool):
        return 'json{kind:bool,value:' + _prolog_atom(str(value)) + '}'
    # Originally, we had int, str return a wrapped item like bool,
    # but removing the wrapper gave 10% performance boost overall.
    if isinstance(value, str):
        return _prolog_atom(value)
    if isinstance(value, int):
        return str(value)
    if isinstance(value, dict):
        return ('json{kind:dict, items:json{' + ','.join(
            (_prolog_atom(key) + ':' + _as_prolog_str_full(value))
            for key, value in value.items()) + '}}')
    if value is None:
        return "json{kind:'None'}"
    if isinstance(value, Exception):
        return "json{kind:'Exception',value:" + _prolog_atom(repr(value)) + '}'
    raise NotImplementedError(f'{value.__class__.__name__}: Unknown value: {value!r}')
