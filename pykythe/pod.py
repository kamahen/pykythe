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
from typing import Any, Dict, Sequence, Text, TypeVar, cast  # pylint: disable=unused-import


class PlainOldData:
    """A base class that acts similarly to collections.namedtuple.

    To use, define your subclass as inheriting from PlainOldData with
    __slots__ set to the allowed attribute names. The subclass should
    also define __init__, to ensure that all the attributes are set
    (it can also use type annotations). There's a trivlal example:
    pykythe_test.SomeData.

    PlainOldData class defines __repr__, __eq__, replace, and _asdict
    (plus as_json_dict), similar in effect to
    collections.namedtuple. Because it uses regular Python classes, it
    isn't immutable; but it's also faster than namedtuple.
    """

    # TODO: https://github.com/python/mypy/issues/4547
    __slots__ = ()  # type: Sequence[str]

    def __init__(self, **kwargs: Any) -> None:
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
        except AttributeError as exc:
            return self.__class__.__name__ + ':*ERROR*' + repr(exc)
        return self.__class__.__name__ + '(' + args + ')'

    def __eq__(self, other: Any) -> bool:
        """Test for equality."""
        return (self is other or (self.__class__ == other.__class__ and all(
            getattr(self, k) == getattr(other, k) for k in self.__slots__)))

    def __hash__(self) -> int:
        raise NotImplementedError(self)  # Because it's not immutuable

    # TODO: def __hash__(self):
    #           return hash(self.__class__) + sum(
    #               hash(getattr(self, a) for a in self.__slots__))
    #   -- but only if the object is immutable

    _SelfType = TypeVar('_SelfType', bound='PlainOldData')

    def _replace(self: _SelfType, **kwargs: Any) -> _SelfType:  # pylint: disable=undefined-variable
        """Make a new object, replacing fields with new values."""
        new_attrs = {
            k: kwargs.pop(k) if k in kwargs else getattr(self, k)
            for k in self.__slots__
        }
        if kwargs:
            raise ValueError('Unknown field names: {!r}'.format(list(kwargs)))
        return self.__class__(  # type: ignore  # TODO: https://github.com/python/mypy/issues/4602
            **new_attrs)

    def _asdict(self) -> Dict[Text, Any]:
        """Return an OrderedDict mapping field names to values."""
        return collections.OrderedDict(
            (k, getattr(self, k)) for k in self.__slots__)

    def as_json_dict(self) -> Dict[Text, Any]:
        """Return an OrderedDict for all non-None fields.

        This doesn't recursively traverse the contents of the object;
        if that is desired, you'll need to write something like
        ast_cooked.AstNode.as_json_dict.
        """
        result = collections.OrderedDict()  # type: Dict[Text, Any]
        for k in self.__slots__:
            value = getattr(self, k)
            if value is not None:
                result[k] = value
        return result
