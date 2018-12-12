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

    __slots__ = []  # Subclass *must* define its own __slots__

    def as_json_dict(self) -> Mapping[Text, Any]:
        # TODO: -> collections.OrderedDict[Text, Any]
        """Return an OrderedDict for all non-None fields.

        This doesn't recursively traverse the contents of the object;
        if that is desired, you'll need to write something like
        PlainOldDataExtended.as_json_dict.
        """
        result = collections.OrderedDict(
        )  # type: collections.OrderedDict[Text, Any]
        for attr in self.__slots__:
            value = getattr(self, attr)
            if value is not None:
                result[attr] = value
        return result

    def as_json_str(self) -> Text:
        return json.dumps(self.as_json_dict())


class PlainOldDataExtended(PlainOldData):
    """PlainOlData that can JSON-ify certain types."""

    def as_json_dict(self) -> Mapping[Text, Any]:
        """Recursively turn a node into a dict for JSON-ification."""
        return self.make_json_dict(self.__class__.__name__)

    def make_json_dict(self, name: Text) -> MutableMapping[Text, Any]:
        result = collections.OrderedDict(
        )  # type: collections.OrderedDict[Text, Any]
        for slot in self.__slots__:
            try:  # TODO: delete (it's for debugging)
                value = getattr(self, slot)
            except AttributeError as exc:
                raise RuntimeError('%r not in %r slots for %r' %
                                   (slot, self.__slots__, self)) from exc
            if value is not None:
                result[slot] = _as_json_dict_full(value)
        return collections.OrderedDict(kind=name, slots=result)


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
    if isinstance(value, int):
        return collections.OrderedDict(kind='int', value=value)
    if isinstance(value, str):
        return collections.OrderedDict(kind='str', value=value)
    if isinstance(value, dict):
        return collections.OrderedDict(
            kind='dict',
            items=collections.OrderedDict((key, _as_json_dict_full(value))
                                          for key, value in value.items()))
    if value is None:
        return collections.OrderedDict(kind='None')
    raise NotImplementedError('{}: Unknown value: {!r}'.format(
        value.__class__.__name__, value))
