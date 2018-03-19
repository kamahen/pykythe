"""Define PlainOldData class.

This is similar in intent to collections.namedtuple, by providing
default implementations of __init__, __repr__, __eq__, _replace,
etc. Unforunately, this doesn't ensure non-mutability (that would be
more work -- if you want it, see https://pypi.python.org/pypi/attrs).

For performance (and to make pylint happy), you should define your own
__init__ (that doesn't call super().__init__).
"""

# pylint: disable=too-few-public-methods

import collections
import json
from lib2to3 import pytree  # For PlainOldDataExtended
from typing import (Any, Mapping, MutableMapping, Sequence, Text, TypeVar)  # pylint: disable=unused-import


class PlainOldData:
    """A base class that acts similarly to collections.namedtuple.

    To use, define your subclass as inheriting from PlainOldData with
    __slots__ set to the allowed attribute names. The subclass should
    also define __init__, to ensure that all the attributes are set
    (it can also use type annotations) ... it should also specify that
    all the args are kwargs (using a bare `*`) to keep compatibility
    with namedtuple. There's a trivial example: pykythe_test.SomeData.

    PlainOldData class defines __repr__, __eq__, replace, and _asdict
    (plus as_json_dict), similar in effect to
    collections.namedtuple. Because it uses regular Python classes, it
    isn't immutable; but it's also faster than namedtuple.

    # TODO: enforce immutability.
    """

    # TODO: https://github.com/python/mypy/issues/4547
    __slots__ = ()  # type: Sequence[str]

    def __init__(self, **kwargs: Any) -> None:  # pragma: no cover
        """Create object with attrs defined by a dict."""
        # *args aren't allowed (Python will raise TypeError)
        try:
            for attr in self.__slots__:  # type: Text
                # Will raise KeyError if a kwargs item isn't in __slots__
                setattr(self, attr, kwargs.pop(attr))
        except KeyError as exc:
            raise ValueError('Missing field: {}'.format(
                ', '.join(repr(a) for a in exc.args))) from exc
        if kwargs:
            raise ValueError('Unknown field names: {!r}'.format(list(kwargs)))

    def __repr__(self) -> Text:
        """Return a nicely formatted representation string."""
        try:
            args = ', '.join('{}={!r}'.format(attr, getattr(self, attr))
                             for attr in self.__slots__)
        except AttributeError as exc:  # pragma: no cover
            return self.__class__.__name__ + ':*ERROR*' + repr(exc)
        return self.__class__.__name__ + '(' + args + ')'

    def __eq__(self, other: Any) -> bool:
        """Test for equality."""
        return (self is other or (self.__class__ == other.__class__ and all(
            getattr(self, attr) == getattr(other, attr)
            for attr in self.__slots__)))

    # __hash__ is not defined: this will result in a TypeError if
    # an attempt is made to use this object as the key to a dict.

    # TODO: def __hash__(self):  # Define this only if the object is immutable
    #           return hash(self.__class__) + sum(
    #               hash(getattr(self, a) for a in self.__slots__))

    # TODO: pytype doesn't like `bound` with a string:
    _SelfType = TypeVar('_SelfType', bound='PlainOldData')

    def _replace(self: _SelfType, **kwargs: Any) -> _SelfType:  # pylint: disable=undefined-variable
        """Make a new object, replacing fields with new values."""
        # The following doesn't depend on the ordering of kwargs
        # because there can't be any dups in it:
        new_attrs = {
            attr: kwargs.pop(attr) if attr in kwargs else getattr(self, attr)
            for attr in self.__slots__
        }
        if kwargs:
            raise ValueError('Unknown field names: {!r}'.format(list(kwargs)))
        return self.__class__(  # type: ignore  # TODO: https://github.com/python/mypy/issues/4602
            **new_attrs)

    def _asdict(self) -> Mapping[Text, Any]:
        """Return an OrderedDict mapping field names to values."""
        return collections.OrderedDict((slot, getattr(self, slot))
                                       for slot in self.__slots__)

    def as_json_dict(
            self
    ) -> Mapping[Text, Any]:  # TODO: -> collections.OrderedDict[Text, Any]
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
    return collections.OrderedDict(
        NOT_POD=value.__class__.__name__, value=value)
