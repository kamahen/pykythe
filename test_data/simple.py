"""Exercise some simple class/attribute bindings.

This code is for development debugging and will change a lot over time
(and eventually disappear).
"""

# TODO: remove the "import", "from" tests that exist elsewhere

#- { @os_path ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0path  ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0os    ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
from os import path as os_path

#- { @sep defines/binding vname("${ROOT_FQN}.test_data.simple.sep", _, _, "", python) }
#- { @#0os        ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @#0path      ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0sep       ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
#- { @sep         ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
from os.path import sep

#- { @os_path_sep defines/binding vname("${ROOT_FQN}.test_data.simple.os_path_sep", _, _, "", python) }
#- { @#0os        ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @#0path      ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0sep       ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
#- { @os_path_sep ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
from os.path import sep as os_path_sep

#- // Note that .xxx doesn't exist
#- { @"."  ref/imports vname("${ROOT_FQN}.test_data",          _, _, "", python) }
#- { @xxx  ref/imports vname("${ROOT_FQN}.test_data.xxx",      _, _, "", python) }
#- { @foo3 ref/imports vname("<unknown>.${ROOT_FQN}.test_data.xxx.foo3", _, _, "", python) }
#- { @foo3 defines/binding vname("${ROOT_FQN}.test_data.simple.foo3", _, _, "", python) }
from .xxx import foo3

# Note that foo2 doesn't exist but we know what the module would be if the
# file(s) did exist.
#- { @foo2 defines/binding vname("${ROOT_FQN}.test_data.simple.foo2", _, _, "", python) }
#- { @foo2 ref/imports vname("${ROOT_FQN}.test_data.foo2", _, _, "", python) }
from . import foo2

# TODO: See builtins_extra.pyi ...
#       Maybe ":" is being handled incorrectly? DO NOT SUBMIT
#- // TODO: Should this be NoneType or None? The problem is that
#- //       mypy and other programs allow "def foo() -> None" as
#- //       meaning "def foo() -> NoneType".
#- { @None ref None=vname("${BUILTINS_FQN}.builtins.None", _, _, "", python) }
None

#- { @False ref vname("${BUILTINS_FQN}.builtins.False", _, _, "", python) }
False

#- { @int ref Int=vname("${BUILTINS_FQN}.builtins.int", _, _, "", python) }
#- { @bit_length ref vname("${BUILTINS_FQN}.builtins.int.bit_length", _, _, "", python) }
int.bit_length()

#- { @bit_length ref vname("${BUILTINS_FQN}.builtins.int.bit_length", _, _, "", python) }
(123).bit_length()


# TODO: move the following to a separate file (see also bindings.py
#       "class C" et al).

#- { @D defines/binding ClassD }
#- { ClassD.node/kind record }
#- { ClassD.subkind class }
class D: pass

#- { @D ref ClassD }
D

#- { @D ref ClassD }
D()

#- { @#0D ref ClassD }
#- { @#1D ref ClassD }
#- { @dd defines/binding FuncDd }
#- { FuncDd.node/kind function }
def dd() -> D: return D()

#- { @dd ref FuncDd }
dd
#- { @dd ref FuncDd }
dd()

# TODO: end of "move to a separate file"


#- { @bar defines/binding Foo_bar }
#- { @#0int ref Int }
#- { @#1int ref Int }
def foo(bar: int) -> int:
    #- { @bar ref Foo_bar }
    #- { @zot defines/binding Foo_zot }
    zot = bar
    #- { @zot ref Foo_zot }
    return zot


#- { @C2 defines/binding C2 }
class C2:
    """A class comment for testing."""
    #- { @__init__ defines/binding _C2_init }
    #- { @self defines/binding C2_init_self }
    #- { @None ref None }
    def __init__(self) -> None:
        """A function comment for testing."""
        #- { @self ref C2_init_self }
        #- { @#0x defines/binding C2_x }
        self.x = 'C2_x'

def make_C2() -> C2: return C2()

#- { @C1 defines/binding C1 }
class C1:
    #- { @__init__ defines/binding _C1_init }
    #- { @self defines/binding C1_init_self }
    #- { @x defines/binding C1_init_x }
    def __init__(self, x: C2) -> None:
        #- { @self ref C1_init_self }
        #- { @#0x defines/binding C1_x }
        #- { @#1x ref C1_init_x }
        self.x = x

def make_C1(x: C2) -> C1: return C1(x)

#- { @c2 defines/binding Global_c2 }
#- { @C2 ref C2 }
#- { Global_c2.node/kind variable }
#- { Global_c2./pykythe/type "[class_type('${ROOT_FQN}.test_data.simple.C2',[])]" }
#- // { @C2 ref C2_init } // TODO
c2 = C2()

#- { @c2 ref Global_c2 }
#- { @x ref C2_x }
c2.x

#- { @c1 defines/binding Global_c1 }
#- { @C1 ref C1 }
#- // { @C1 ref C1_init } // TODO
#- // TODO: calls C1_init (etc.)
#- { @C2 ref C2 }
#- // { @C2 ref C2_init } // TODO
c1 = C1(C2())

#- { @c1 ref Global_c1 }
#- { @#0x ref C1_x }
#- { @#1x ref C2_x }
assert c1.x.x == 'C2_x'

#- { @c3 defines/binding _Global_c3 }
#- { @c1 ref Global_c1 }
#- { @#0x ref C1_x }
#- { @#1x ref C2_x }
c3 = c1.x.x

#- { @c1 ref Global_c1 }
#- { @#0x ref C1_x }
#- { @#1x defines/binding C2_x }
c1.x.x = 'C2_x_modified'

m_c2 = make_C2()
#- { @x ref C2_x }
m_c2.x

#- { @#0x ref C1_x }
#- { @#1x ref C2_x }
make_C1(make_C2()).x.x

# TODO: The following had syntax errors (lib2to3 bug)
#       https://bugs.python.org/issue33064
def foo(bar, *baz,): pass
def foo(bar, **baz,): pass
def foo(bar, *, baz,): pass
