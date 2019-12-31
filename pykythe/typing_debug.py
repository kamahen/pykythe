"""Variants from the typing module, for debugging."""

# TODO: delete this when no longer needed.

from typing import Any, Sequence, Type, Tuple, TypeVar, Union

# This definition of `cast` is the same as typing.cast, except it
# has code to verify type.
# TODO: replace by typing.cast

_T = TypeVar('_T')


def cast(typ: Type[_T], val: Any, extra_info: Any = None) -> _T:
    assert isinstance(val, typ), dict(type=typ, val=val, extra_info=extra_info)
    return val


def assert_all_isinstance(typ: Union[type, Tuple[type, ...]],
                          val: Sequence[Any],
                          extra_info: Any = None) -> None:
    for v in val:
        assert isinstance(v, typ), dict(type=typ, val=val, extra_info=extra_info)
