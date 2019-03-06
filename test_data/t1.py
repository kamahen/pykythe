# TODO: delete this (it's a simplified part of simple.py)

#- { @C2 defines/binding C2? }
class C2:
    """A class comment for testing."""
    #- { @__init__ defines/binding _C2_init? }
    #- { @self defines/binding C2_init_self? }
    #- { @None ref None? }  // TODO: should be builtins.None
    def __init__(self) -> None:
        """A function comment for testing."""
        #- { @self ref C2_init_self? }
        #- { @#0z defines/binding C2_z? }
        self.z = 'C2_z'


#- { @C1 defines/binding C1? }
class C1:
    #- { @__init__ defines/binding _C1_init? }
    #- { @self defines/binding C1_init_self? }
    #- { @x defines/binding C1_init_x? }
    #- { @str ref Str? }  // TODO: should be builtins.str
    def __init__(self, x: C2, y: str) -> None:
        #- { @self ref C1_init_self? }
        #- { @#0x defines/binding C1_x? }
        #- { @#1x ref C1_init_x? }
        self.x = x
        self.y = y

#- { @c1 defines/binding C1var? }
c1 = C1(C2(), 'xyz')

#- { @c1_x defines/binding C1_x_var? }
#- { @#1c1 ref C1var }
#- { @#1x ref C1_x }
c1_x = c1.x

#- { @c1_x ref C1_x_var }
#- { @z ref C2_z }
c1_x.z

#- { @c1 ref C1var }
#- { @x ref C1_x }
#- { @z ref C2_z }
c1.x.z

#- { @C1 ref C1 }
#- { @x ref C1_x }
#- { @z ref C2_z }
C1(C2(), 'abc').x.z
