"""Exercise some simple class/attribute bindings.

This code is for development debugging and will change a lot over time
(and eventually disappear).
"""


#- @C2 defines/binding C2
class C2:
    """A class comment for testing."""
    #- @__init__ defines/binding C2_init
    #- @self defines/binding C2_init_self
    #- // TODO: point to self's type: C2
    def __init__(self) -> None:
        """A function comment for testing."""
        #- @self ref C2_init_self
        #- @#0x defines/binding C2_x
        self.x = 'C2_x'

#- @C1 defines/binding C1
class C1:
    #- @__init__ defines/biding C1_init
    #- @self defines/binding C1_init_self
    #- @x defines/binding C1_init_x
    def __init__(self, x: C2) -> None:
        #- @self ref C1_init_self
        #- @#0x defines/binding C1_x
        #- @#1x ref @C1_init_x
        self.x = x

#- @c defines/binding Global_c2
#- @C2 ref C2
#- // @C2 ref C2_init
c2 = C2()

#- @c2 ref Global_c
#- @x ref C2_x
c2.x

#- @c1 defines/binding Global_c1
#- @C1 ref C1
#- // @C1 ref C1_init
#- @C2 ref C2
#- //@C2 ref C2_init
c1 = C1(C2())

#- @c1 ref Global_c1
#- @#0x ref C1_x
#- @#1x ref C2_x
assert c1.x.x == 'C2_x'

#- @c1 ref Global_c1
#- @#0x ref C1_x
#- @#1x defines/binding C2_x
c1.x.x = 'C2_x_modified'

# TODO: The following have syntax errors (lib2to3 bug)
#       https://bugs.python.org/issue33064
# def foo(bar, *baz,): pass
# def foo(bar, **baz,): pass
# def foo(bar, *, baz,): pass
