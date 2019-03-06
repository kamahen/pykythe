# C3 algorithm
# https://en.wikipedia.org/wiki/C3_linearization
# TODO: need to verify that we compute the MRO given in the assertion

# See also additional examples in
# https://www.python.org/download/releases/2.3/mro/

# Editorial comment: this code is unnecessarily opaque, especially the
# "remove cand" part, which depends on Python passing around pointers
# for lists. A cleaner implementation is in c3_a.py, which is in a
# pure functional form.

# Python implementation from Python docs, slightly reformatted and
# modified to Python 3:


def mro(C):
    "Compute the class precedence list (mro) according to C3"
    return mro_merge([[C]] + list(map(mro, C.__bases__)) + [list(C.__bases__)])


def mro_merge(seqs):
    res = []
    while True:
        nonemptyseqs = [seq for seq in seqs if seq]
        if not nonemptyseqs:
            return res
        for seq in nonemptyseqs:  # find merge candidates among seq heads
            cand = seq[0]
            nothead = [s for s in nonemptyseqs if cand in s[1:]]
            if nothead:
                cand = None  #reject candidate
            else:
                break
        if not cand:
            raise "Inconsistent hierarchy"
        res.append(cand)
        for seq in nonemptyseqs:  # remove cand
            if seq[0] == cand:
                del seq[0]


##### TESTS #####


class Type(type):

    def __repr__(cls):
        return cls.__name__


class O(object, metaclass=Type):
    pass


class A(O):
    pass


class B(O):
    pass


class C(O):
    pass


class D(O):
    pass


class E(O):
    pass


class K1(A, B, C):
    pass


class K2(D, B, E):
    pass


class K3(D, A):
    pass


class Z(K1, K2, K3):
    pass


assert Z.mro() == [Z, K1, K2, K3, D, A, B, C, E, O, object]


def print_mro(C):
    c_mro = mro(C)
    print('MRO[%s]=%s' % (C, c_mro))
    print('MRO[%s]=%s' % (C, C.mro()))
    assert str(c_mro) == str(C.mro()), [c_mro, C.mro()]


for Class in Z, K1, K2, K3, D, A, B, C, E, O:
    print_mro(Class)
    print()
