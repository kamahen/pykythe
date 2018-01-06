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

    __slots__ = ()

    def __init__(self, *args, **kwargs) -> None:
        """Create object with attrs defined by a dict."""
        if args:
            raise ValueError('POD takes only kwargs')
        try:
            for attr in self.__slots__:
                # Will raise KeyError if a kwargs item isn't in __slots__
                setattr(self, attr, kwargs.pop(attr))
        except KeyError as exc:
            raise ValueError('Missing field: {}'.format(
                ', '.join(repr(a) for a in exc.args))) from exc
        if kwargs:
            raise ValueError('Unknown field names: {!r}'.format(list(kwargs)))

    def __repr__(self):
        """Return a nicely formatted representation string."""
        args = ', '.join('{}={!r}'.format(attr, getattr(self, attr))
                         for attr in self.__slots__)
        return self.__class__.__name__ + '(' + args + ')'

    def __eq__(self, other):
        """Test for equality."""
        return (self is other or (self.__class__ == other.__class__ and all(
            getattr(self, k) == getattr(other, k) for k in self.__slots__)))

    def _replace(self, **kwargs):
        """Make a new object, replacing fields with new values."""
        new_attrs = {
            k: kwargs.pop(k) if k in kwargs else getattr(self, k)
            for k in self.__slots__
        }
        if kwargs:
            raise ValueError('Unknown field names: {!r}'.format(list(kwargs)))
        return self.__class__(**new_attrs)

    def _asdict(self):
        """Return an OrderedDict mapping field names to values."""
        return collections.OrderedDict(
            (k, getattr(self, k)) for k in self.__slots__)

    def as_json_dict(self):
        """Return an OrderedDict for all non-None fields."""
        result = collections.OrderedDict()
        for k in self.__slots__:
            value = getattr(self, k)
            if value is not None:
                result[k] = value
        return result
