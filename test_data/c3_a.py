# c.py modified to pure functional style (Python 3).

# C3 algorithm
# https://en.wikipedia.org/wiki/C3_linearization
# TODO: need to verify that we compute the MRO given in the assertion

# See also additional examples in
# https://www.python.org/download/releases/2.3/mro/


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
