"""Exercise some simple class/attribute bindings.

This code is for development debugging and will change a lot over time
(and eventually disappear).
"""

# TODO - remove these import's
#- { @foo4 defines/binding Foo4?  } // TODO: needs to be implemented
#- { @foo_bar5 defines/binding Foo5?  } // TODO: needs to be implemented
from ....yyy import foo4, foo5 as foo_bar5
#- { @sep defines/binding Sep? }
from os.path import sep
#- { @foo3 defines/binding Foo3? }
from .xxx import foo3
#- { @foo1 defines/binding Foo1? }
from .. import foo1
#- { @foo2 defines/binding Foo2? }
from . import foo2
#- { @xos defines/binding Xos? }  // TODO: needs full implementation
import xos
#- // !{ @path defines/binding _ }  // TODO: check that there's no anchor either, also for other "from" and "import"
#- { @xos defines/binding _Xos2?}  // TODO: check that there's no anchor either
import xos.path
#- { @os_path defines/binding OsPath? }  // TODO: needs full implementation
import xos.path2 as os_path

#- @foo4 ref Foo4?
foo4

#- @foo4 ref Foo4?
#- @x ref Foo4X?
foo4.x

fff = foo4

#- @x ref Foo4X?
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
