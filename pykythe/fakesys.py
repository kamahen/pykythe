"""Bare minimal "sys", for safely evaluating strings."""

from typing import Any, Optional, Tuple, Text

import sys
from dataclasses import dataclass
from . import pod


@dataclass(frozen=True)
class EvalResult(pod.PlainOldDataExtended):
    """The result of FakeSys.eval."""

    result: Any  # Optional[bool] but in theory could be anything
    exception: Optional[Exception]
    __slots__ = ['result', 'exception']


@dataclass(frozen=True)
class FakeSysWithVersionInfo:
    """A very limited `sys`, with only `version_info`.

    This is used to safely evaluate a string (see `FakeSys`).
    """

    version_info: Tuple[int, int, int, str, int]
    __slots__ = ['version_info']


@dataclass(frozen=True)
class FakeSys:
    """Provide a bare minimum of sys functionality for eval.

    There are situations where we need to statically evaluate an
    if-then-else (particularly in builtins.pyi) to avoid inconsistent
    Kythe node/kind values. Typically, these are tests on
    sys.version_info, so this class is used to do the evaluation,
    returning True, False, or "can't compute".

    This code uses `eval`, which would normally be a security
    hole. However, the local and global variables are specified to be
    only a limited version of `sys`, so the evaluation is safe.
    """

    fake_sys_with_version_info: FakeSysWithVersionInfo
    __slots__ = ['fake_sys_with_version_info']

    def eval(self, expr: Text) -> EvalResult:
        try:
            # The following is safe because global and local
            # environments are constrained to a limited version of
            # `sys`.
            result = eval(expr, {}, {'sys': self.fake_sys_with_version_info})  # pylint: disable=eval-used
            return EvalResult(result=result, exception=None)
        except Exception as exc:  # pylint: disable=broad-except
            return EvalResult(result=None, exception=exc)


# TODO: use __main__.args.python_version (and change that to be a triple)
FAKE_SYS = FakeSys(FakeSysWithVersionInfo(sys.version_info))  # TODO: parameterize this
