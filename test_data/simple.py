"""Exercise some simple class/attribute bindings.

This code is for development debugging and will change a lot over time
(and eventually disappear).
"""

#- { @#0os ref/file XXX2? }  // vname("", "test-corpus", "test-root", "${ROOT_DIR}/typeshed/stdlib/3/os/__init__.py", "")
#- // { @#0path ref OsPath1? }  // TODO
from os import path as os_path

# TODO - remove these import's
#- { @foo4 defines/binding Foo4?  } // TODO: needs to be implemented
#- { @foo_bar5 defines/binding Foo5?  } // TODO: needs to be implemented
#- { @". . . . yyy .zot" ref/file vname("", "test-corpus", "test-root", "${ROOT_DIR}/test_data/simple/../../../yyy/zot", "") }
#- // TODO: should we have a node/kind fact for referenced files, even
#- //       if they don't exist?
#- //       { DotDotDotDotYyyZot.node/kind file }  // TODO: remove this?
from . . . . yyy .zot import foo4, foo5 as foo_bar5

#- { @"os.path" ref/file vname("", "test-corpus", "test-root", "${TYPESHED_DIR}/stdlib/3/os/path.pyi", "") }
#- { @sep defines/binding Sep? }
#- { @sep ref/imports OS_PATH_SEP? }
from os.path import sep

#- { @".xxx" ref/file vname("", "test-corpus", "test-root", "${ROOT_DIR}/test_data/simple/xxx", "") }
#- { @foo3 defines/binding Foo3? }
from .xxx import foo3

#- { @".." ref/file vname("", "test-corpus", "test-root", "${ROOT_DIR}/test_data/simple/..", "") }
#- { @foo1 defines/binding Foo1? }
#- // { @foo1 ref/imports vname("${ROOT_FQN}.test_data.foo1", _, _, "", python) }
#- { @foo1 ref/imports FOO1? }
from .. import foo1

#- { @"." ref/file vname("", "test-corpus", "test-root", "${ROOT_DIR}/test_data/simple.py", "") }
#- { @foo2 defines/binding vname("${ROOT_FQN}.test_data.simple.foo2", _, _, "", python) }
#- { @foo2 ref/imports FOO2? } // TODO: is ${ROOT_DIR}/test_data/simple.py.foo2 should be ${ROOT_FQN}.test_data.simple.test_data.foo2
from . import foo2

#- { @os defines/binding Os? }  // TODO: needs full implementation
import os

#- // !{ @path defines/binding _ }  // TODO: check that there's no anchor either, also for other "from" and "import"
#- { @os defines/binding _Os2?}  // TODO: check that there's no anchor either
import os.path

#- { @os_path defines/binding OsPath? }  // TODO: needs full implementation
import os.path2 as os_path

#- @foo4 ref Foo4=vname("${ROOT_FQN}.test_data.simple.foo4", _, _, "", python)
foo4

#- @foo4 ref Foo4
#- // TODO - Foo4X = App(vname, (/tmp/pykythe_test/SUBST/test_data/simple/../../../yyy/zot/foo4::x, test-corpus, test-root, "", python))
#- @x ref Foo4X?  // vname("${ROOT_FQN}.test_data.simple.foo4.x", _, _, "", python)
foo4.x

fff = foo4

#- @x ref Foo4X
fff.x

# TODO: add some simple tests of the imports (e.g. foo4.bar, foo_bar5.bar)

#- @None defines/binding None
None = ...  # TODO: using builtin.pyi
#- @int defines/binding Int
int = float = ...   # TODO: use builtins.pyi


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

#- @dd ref FuncDd
dd
#- @dd ref FuncDd
dd()

# TODO: end of "move to a separate file"


#- @bar defines/binding Foo_bar
#- @#0int ref Int
#- @#1int ref Int
def foo(bar: int) -> int:
    #- @bar ref Foo_bar
    #- @zot defines/binding Foo_zot
    zot = bar
    #- @zot ref Foo_zot
    return zot


#- @C2 defines/binding C2
class C2:
    """A class comment for testing."""
    #- @__init__ defines/binding _C2_init
    #- @self defines/binding C2_init_self
    #- @None ref None
    def __init__(self) -> None:
        """A function comment for testing."""
        #- @self ref C2_init_self
        #- @#0x defines/binding C2_x
        self.x = 'C2_x'

def make_C2() -> C2: return C2()

#- @C1 defines/binding C1
class C1:
    #- @__init__ defines/binding _C1_init
    #- @self defines/binding C1_init_self
    #- @x defines/binding C1_init_x
    def __init__(self, x: C2) -> None:
        #- @self ref C1_init_self
        #- @#0x defines/binding C1_x
        #- @#1x ref C1_init_x
        self.x = x

def make_C1(x: C2) -> C1: return C1(x)

#- @c2 defines/binding Global_c2
#- @C2 ref C2
#- Global_c2.node/kind variable
#- // @C2 ref C2_init
c2 = C2()

#- @c2 ref Global_c2
#- @x ref C2_x
c2.x

#- @c1 defines/binding Global_c1
#- @C1 ref C1
#- // @C1 ref C1_init
#- @C2 ref C2
#- // @C2 ref C2_init
c1 = C1(C2())

#- @c1 ref Global_c1
#- @#0x ref C1_x
#- @#1x ref C2_x
assert c1.x.x == 'C2_x'

#- @c3 defines/binding _Global_c3
#- @c1 ref Global_c1
#- @#0x ref C1_x
#- @#1x ref C2_x
c3 = c1.x.x

#- @c1 ref Global_c1
#- @#0x ref C1_x
#- @#1x defines/binding C2_x
c1.x.x = 'C2_x_modified'

m_c2 = make_C2()
#- @x ref C2_x
m_c2.x

#- @#0x ref C1_x
#- @#1x ref C2_x
make_C1(make_C2()).x.x

# TODO: The following have syntax errors (lib2to3 bug)
#       https://bugs.python.org/issue33064
# def foo(bar, *baz,): pass
# def foo(bar, **baz,): pass
# def foo(bar, *, baz,): pass
