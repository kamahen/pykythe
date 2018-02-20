"""Variants from the typing module, for debugging.

TODO: remove this when no longer needed.
"""

from typing import Any, Type, Tuple, TypeVar, Union

# This definition of `cast` is the same as typing.cast, except it
# verifies the type:
# TODO: replace by typing.cast

_T = TypeVar('_T')


def cast(typ: Type[_T], val: Any) -> _T:
    assert isinstance(val, typ), (typ, val)
    return val


def assert_all_isinstance(typ: Union[Type, Tuple[Type, ...]],
                          val: Any) -> None:
    assert all(isinstance(v, typ) for v in val)
