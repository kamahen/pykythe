# c.py modified to pure functional style (Python 3).

# C3 algorithm
# https://en.wikipedia.org/wiki/C3_linearization
# TODO: need to verify that we compute the MRO given in the assertion

# See also additional examples in
# https://www.python.org/download/releases/2.3/mro/

# This is the code in c3.py, rewritten in a pure functional style
# and removing the unnecessary opaqueness of the original (q.v.).

# In addition, the tests have been expanded to check that the
# appropriate class attributes are accessed, rather than just using
# mro().


def mro(C):
    "Compute the class precedence list (mro) according to C3"
    return mro_merge([[C]] + list(map(mro, C.__bases__)) + [list(C.__bases__)])


def mro_merge(seqs):
    seqs = [seq for seq in seqs if seq]
    if not seqs:
        return []
    cand = find_merge_candidates_among_seq_heads(seqs)
    if not cand:
        raise "Inconsistent hierarchy"
    # remove cand from seq:
    seqs = [seq[1:] if seq[0] == cand else seq for seq in seqs]
    return [cand] + mro_merge(seqs)


def find_merge_candidates_among_seq_heads(seqs):
    if not seqs:
        return None
    cand = seqs[0][0]
    if any(cand in s[1:] for s in seqs):
        return find_merge_candidates_among_seq_heads(seqs[1:])
    else:
        return cand


##### TESTS #####


class Type(type):

    def __repr__(cls):
        return cls.__name__


#- { @Type ref _ }  // Ensure that this is processed, even though it's not used
class O(object, metaclass=Type):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _O_F0 }
        self.f0 = 'O_F0'
        #- { @f1 defines/binding _O_F1 }
        self.f1 = 'O_F1'
        #- { @f2 defines/binding _O_F2 }
        self.f2 = 'O_F2'
        #- { @f3 defines/binding _O_F3 }
        self.f3 = 'O_F3'
        #- { @f4 defines/binding _O_F4 }
        self.f4 = 'O_F4'
        #- { @f5 defines/binding _O_F5 }
        self.f5 = 'O_F5'
        #- { @f6 defines/binding _O_F6 }
        self.f6 = 'O_F6'
        #- { @f7 defines/binding _O_F7 }
        self.f7 = 'O_F7'
        #- { @f8 defines/binding _O_F8 }
        self.f8 = 'O_F8'
        #- { @f9 defines/binding _O_F9 }
        self.f9 = 'O_F9'


class A(O):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _A_F0 }
        self. f0 =               'A_F0'
        #- { @f1 defines/binding _A_F1 }
        self. f1 =               'A_F1'
        #- { @f2 defines/binding _A_F2 }
        self. f2 =               'A_F2'
        #- { @f3 defines/binding _A_F3 }
        self. f3 =               'A_F3'
        #- { @f4 defines/binding _A_F4 }
        self. f4 =               'A_F4'
        #- { @f5 defines/binding  A_F5 }
        self. f5 =               'A_F5'


class B(O):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _B_F0 }
        self. f0 =               'B_F0'
        #- { @f1 defines/binding _B_F1 }
        self. f1 =               'B_F1'
        #- { @f2 defines/binding _B_F2 }
        self. f2 =               'B_F2'
        #- { @f3 defines/binding _B_F3 }
        self. f3 =               'B_F3'
        #- { @f4 defines/binding _B_F4 }
        self. f4 =               'B_F4'
        #- { @f5 defines/binding _B_F5 }
        self. f5 =               'B_F5'
        #- { @f6 defines/binding  B_F6 }
        self. f6 =               'B_F6'


class C(O):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _C_F0 }
        self. f0 =               'C_F0'
        #- { @f1 defines/binding _C_F1 }
        self. f1 =               'C_F1'
        #- { @f2 defines/binding _C_F2 }
        self. f2 =               'C_F2'
        #- { @f3 defines/binding _C_F3 }
        self. f3 =               'C_F3'
        #- { @f4 defines/binding _C_F4 }
        self. f4 =               'C_F4'
        #- { @f5 defines/binding _C_F5 }
        self. f5 =               'C_F5'
        #- { @f6 defines/binding _C_F6 }
        self. f6 =               'C_F6'
        #- { @f7 defines/binding  C_F7 }
        self. f7 =               'C_F7'


class D(O):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _D_F0 }
        self. f0 =               'D_F0'
        #- { @f1 defines/binding _D_F1 }
        self. f1 =               'D_F1'
        #- { @f2 defines/binding _D_F2 }
        self. f2 =               'D_F2'
        #- { @f3 defines/binding _D_F3 }
        self. f3 =               'D_F3'
        #- { @f4 defines/binding  D_F4 }
        self. f4 =               'D_F4'


class E(O):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _E_F0 }
        self. f0 =               'E_F0'
        #- { @f1 defines/binding _E_F1 }
        self. f1 =               'E_F1'
        #- { @f2 defines/binding _E_F2 }
        self. f2 =               'E_F2'
        #- { @f3 defines/binding _E_F3 }
        self. f3 =               'E_F3'
        #- { @f4 defines/binding _E_F4 }
        self. f4 =               'E_F4'
        #- { @f5 defines/binding _E_F5 }
        self. f5 =               'E_F5'
        #- { @f6 defines/binding _E_F6 }
        self. f6 =               'E_F6'
        #- { @f7 defines/binding _E_F7 }
        self. f7 =               'E_F7'
        #- { @f8 defines/binding  E_F8 }
        self. f8 =               'E_F8'



class K1(A, B, C):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _K1_F0 }
        self. f0 =               'K1_F0'
        #- { @f1 defines/binding  K1_F1 }
        self. f1 =               'K1_F1'


class K2(D, B, E):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _K2_F0 }
        self. f0 =               'K2_F0'
        #- { @f1 defines/binding _K2_F1 }
        self. f1 =               'K2_F1'
        #- { @f2 defines/binding  K2_F2 }
        self. f2 =               'K2_F2'


class K3(D, A):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding _K3_F0 }
        self. f0 =               'K3_F0'
        #- { @f1 defines/binding _K3_F1 }
        self. f1 =               'K3_F1'
        #- { @f2 defines/binding _K3_F2 }
        self. f2 =               'K3_F2'
        #- { @f3 defines/binding  K3_F3 }
        self. f3 =               'K3_F3'


class Z(K1, K2, K3):

    def __init__(self):
        super().__init__()
        #- { @f0 defines/binding Z_F0 }
        self. f0 =              'Z_F0'


assert Z.mro() == [Z, K1, K2, K3, D, A, B, C, E, O, object]


def print_mro(C):
    c_mro = mro(C)
    print('MRO[%s]=%s' % (C, c_mro))
    print('MRO[%s]=%s' % (C, C.mro()))
    assert str(c_mro) == str(C.mro()), [c_mro, C.mro()]


for Class in Z, K1, K2, K3, D, A, B, C, E, O:
    print_mro(Class)
    print()

# MRO[Z]=[Z, K1, K2, K3, D, A, B, C, E, O, <class 'object'>]
#         0  1   2   3   4  5  6  7  8  9
# MRO[K1]=[K1, A, B, C, O, <class 'object'>]
# MRO[K2]=[K2, D, B, E, O, <class 'object'>]
# MRO[K3]=[K3, D, A, O, <class 'object'>]
# MRO[D]=[D, O, <class 'object'>]
# MRO[A]=[A, O, <class 'object'>]
# MRO[B]=[B, O, <class 'object'>]
# MRO[C]=[C, O, <class 'object'>]
# MRO[E]=[E, O, <class 'object'>]
# MRO[O]=[O, <class 'object'>]

#- { @z defines/binding Z }
z = Z()
k1 = K1()
k2 = K2()
k3 = K3()
d = D()
a = A()
b = B()
c = C()
e = E()
o = O()

#- { @z ref Z }
#- {    @f0 ref Z_F0 }
assert z.f0 == 'Z_F0'

#- {    @f1 ref K1_F1 }
assert z.f1 == 'K1_F1'

#- {    @f2 ref K2_F2 }
assert z.f2 == 'K2_F2'

#- {     @f3 ref K3_F3 }
assert k3.f3 == 'K3_F3'

#- {    @f3 ref K3_F3 }
assert z.f3 == 'K3_F3'

#- { @z ref Z }
#- {    @f4 ref D_F4 }
assert z.f4 == 'D_F4'

#- {    @f5 ref A_F5 }
assert z.f5 == 'A_F5'

#- {    @f6 ref B_F6 }
assert z.f6 == 'B_F6'

#- {    @f7 ref C_F7 }
assert z.f7 == 'C_F7'

#- {    @f7 ref C_F7 }
assert c.f7 == 'C_F7'

#- {    @f8 ref E_F8 }
assert e.f8 == 'E_F8'

#- {    @f8 ref E_F8 }
assert z.f8 == 'E_F8'

