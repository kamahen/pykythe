"""Define PlainOldData class.

The name of this module is misleading -- its original use has been
superseded by Python 3.7's dataclasses. Instead, it acts as a kind of
mixin class for adding a few common methods for dealing with JSON.
"""

# TODO: rename this module and its classes.

# pylint: disable=too-few-public-methods

import collections
import json
from lib2to3 import pytree  # For PlainOldDataExtended
from typing import (  # pylint: disable=unused-import
    Any, Mapping, MutableMapping, Sequence, Text, TypeVar)


class PlainOldData:
    """A mixin class that adds JSON methods."""

    # Subclass *must* define its own __slots__
    __slots__ = []  # type: Sequence[Text]

    def as_json_dict(self) -> Mapping[Text, Any]:
        # TODO: -> collections.OrderedDict[Text, Any]
        """Return an OrderedDict for all non-None fields.

        This doesn't recursively traverse the contents of the object;
        if that is desired, you'll need to write something like
        PlainOldDataExtended.as_json_dict.
        """
        slots = collections.OrderedDict(
        )  # type: collections.OrderedDict[Text, Any]
        for attr in self.__slots__:
            value = getattr(self, attr)
            if value is not None:
                slots[attr] = value
        return slots

    def as_json_str(self) -> Text:
        return json.dumps(self.as_json_dict())

    def as_prolog_str(self) -> Text:
        """Return a string that represents a Prolog dict.

        This doesn't recursively traverse the contents of the object;
        if that is desired, you'll need to write something like
        PlainOldDataExtended.as_json_str.
        """
        result = 'json{'
        sep = ''
        for attr in self.__slots__:
            value = getattr(self, attr)
            if value is not None:
                result += f'{sep}{_prolog_atom(attr)}:{_prolog_atom(value)}'
                slots[attr] = value
                sep = ','
        return result + '}'


class PlainOldDataExtended(PlainOldData):
    """PlainOlData that can JSON-ify certain types."""

    def as_json_dict(
            self
    ) -> Mapping[Text, Any]:  # OrderedDict is actually MutableMapping
        """Recursively turn a node into a dict for JSON-ification."""
        slots = collections.OrderedDict(
        )  # type: collections.OrderedDict[Text, Any]
        for slot in self.__slots__:
            try:  # TODO: delete (try/except is for debugging)
                value = getattr(self, slot)
            except AttributeError as exc:
                raise RuntimeError('%r not in %r slots for %r' %
                                   (slot, self.__slots__, self)) from exc
            if value is not None:
                slots[slot] = _as_json_dict_full(value)
        return collections.OrderedDict(
            kind=self.__class__.__name__, slots=slots)

    def as_prolog_str(self) -> Text:
        """Recursively turn a node into a Prolog term."""
        result = ('json{kind:' + _prolog_atom(self.__class__.__name__) +
                  ',slots:json{')
        sep = ''
        for slot in self.__slots__:
            value = getattr(self, slot)
            if value is not None:
                result += f'{sep}{_prolog_atom(slot)}:{_as_prolog_str_full(value)}'
                sep = ','
        return result + '}}'


def _prolog_atom(value):
    """Wrap a string so that it's a valid Prolog atom."""
    return "'" + value.replace('\\', '\\\\').replace("'", r"\'") + "'"


def _as_json_dict_full(value: Any) -> Any:
    """Recursively turn an object into a dict for JSON-ification."""
    # pylint: disable=too-many-return-statements
    if isinstance(value, PlainOldData):
        return value.as_json_dict()
    if isinstance(value, list):
        return [_as_json_dict_full(v) for v in value]
    if isinstance(value, pytree.Leaf):
        return collections.OrderedDict(
            kind='Leaf',
            leaf_kind=value.type,
            value=value.value,
            prefix=value.prefix,
            lineno=value.lineno,
            column=value.column)
    if isinstance(value, bool):
        return collections.OrderedDict(kind='bool', value=str(value))
    # Originally, we had int, str return a wrapped item like bool,
    # but removing the wrapper gave 10% performance boost overall.
    if isinstance(value, str):
        return value
    if isinstance(value, int):
        return value
    if isinstance(value, dict):
        return collections.OrderedDict(
            kind='dict',
            items=collections.OrderedDict((key, _as_json_dict_full(value))
                                          for key, value in value.items()))
    if value is None:
        return collections.OrderedDict(kind='None')
    if isinstance(value, Exception):
        return collections.OrderedDict(kind='Exception', value=repr(value))
    raise NotImplementedError(
        f'{value.__class__.__name__}: Unknown value: {value!r}')


def _as_prolog_str_full(value: Any) -> Text:
    """Recursively turn an object into a python term."""
    # pylint: disable=too-many-return-statements
    if isinstance(value, PlainOldData):
        return value.as_prolog_str()
    if isinstance(value, list):
        return '[' + ','.join(_as_prolog_str_full(v) for v in value) + ']'
    if isinstance(value, pytree.Leaf):
        return 'json{' + ','.join(
            'kind:Leaf', 'leaf_kind:' + value.type, 'value:' +
            _prolog_atom(value.value), 'prefix:' + _prolog_atom(value.prefix),
            'lineno:' + str(value.lineno), 'column:' + str(value.column)) + '}'
    if isinstance(value, bool):
        return 'json{kind:bool, value:' + _prolog_atom(str(value)) + '}'
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
    raise NotImplementedError(
        f'{value.__class__.__name__}: Unknown value: {value!r}')
