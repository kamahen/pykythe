# TODO: mark this string as childof the package
""" Tests for the childof relation."""

#- { Pkg=vname("${ROOT_FQN}.test_data.childof", _, _, "", python).node/kind package }
#- { File=vname("", "CORPUS", "ROOT", "${ROOT_DIR}/test_data/childof.py", "").node/kind file }
#- { File childof Pkg }

from typing import Optional

#- { @one_global defines/binding OneGlobal }
#- { OneGlobal childof Pkg }
one_global: int
#- { @two_global defines/binding TwoGlobal }
#- { TwoGlobal childof Pkg }
two_global = 'xyz'
#- { @three_global defines/binding ThreeGlobal }
#- { ThreeGlobal childof Pkg }
three_global: Optional[str] = None

#- { @C1 defines/binding C1 }
#- { C1 childof Pkg }
class C1:
    #- { @f1 defines/binding C1_f1 }
    #- { C1_f1 childof C1 }
    f1: int
    #- { @f2 defines/binding C1_f2 }
    #- { C1_f2 childof C1 }
    f2 = 'xyz'
    #- { @f3 defines/binding C1_f3 }
    #- { C1_f3 childof C1 }
    f3: Optional[str] = None

    #- { @__init__ defines/binding C1_init }
    #- { C1_init childof C1 }
    #- { @f4 defines/binding C1_init_param_f4 }
    #- { C1_init_param_f4 childof C1_init }
    def __init__(self, f4):
        #- { @#0f4 defines/binding _C1_f4 }
        #- // { C1_f4 childof X_C1? } // TODO: should this exist? (cf f3)  DO NOT SUBMIT
        #- { @#1f4 ref C1_init_param_f4 }
        self.f4 = f4
        #- // { @f1 defines/binding C1_f1 }
        #- // { C1_f1 childof C1 } // TODO: should this exist? (cf f3)
        self.f1 = '---'
        #- { @xf4 defines/binding C1_xf4 }
        #- { C1_xf4 childof C1_init }
        xf4 = f4 + 1
        self.xf4 = xf4

    def get_f2(self):
        return self.f2

    def get_f4(self):
        def inside():
            return self.f4
        return inside()

    def values(self):
        return [self.f1, self.f2, self.f3, self.f4]

    class C2:
        g1: int
        g2 = 'abc'
        g3: Optional[str] = None

        def __init__(self, g4):
            self.g4 = g4
            self.g1 = '***';

        def values(self):
            try:
                # self.f1 is an error
                return [self.g1, self.g2, self.g3, self.g4, self.f1]
            except Exception as e:
                return [self.g1, self.g2, self.g3, self.g4, e]

        def not_a_method(x):
            return x + C1.f2


c1 = C1(111)
print(c1.get_f4())
print(c1.values())

#- // TODO: should there be a childof for f4?
c1.f4 = '==='

c2 = c1.C2(666)
print(c2.values())

print(C1.C2.not_a_method('--'))
