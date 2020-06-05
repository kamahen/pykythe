# DO NOT SUBMIT - need additional verifier checks when list comprehensions
#                 are handled better

class C:
    def __init__(self):
        #- { @x defines/binding C_x }
        #- { @capitalize ref vname("${BUILTINS_FQN}.builtins.str.capitalize", _, _, "", python) }
        #- !{ @capitalize tagged _ }
        self.x = 'abc'.capitalize()
        #- { @swapcase ref vname("${BUILTINS_FQN}.builtins.str.swapcase", _, _, "", python) }
        #- !{ @swapcase tagged _ }
        self.y = self.x.swapcase()

class D:
    def __init__(self):
        #- { @x defines/binding D_x }
        self.x = 'abc'


def foo() -> None:
    l = [C()]

    #- { @x ref C_x }
    #- { @x ref D_x }   // DO NOT SUBMIT - should not be true
    m = [c.x for c in l]
    n = m[0]
    print(n.swapcase())


foo()
