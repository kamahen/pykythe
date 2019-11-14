"""Exercise various bindings/ref patterns."""

# See comment in py3_test_grammar.py about the "{...}"s.

# TODO: These only test Python 3; need similar tests for Python2
# (e.g., list comprehensions "leak" in PYthon2)

#- { Pkg=vname("${ROOT_FQN}.test_data.bindings", "test-corpus", "test-root", "", python).node/kind package }
#- { File=vname("", "test-corpus", "test-root", "${ROOT_DIR}/test_data/bindings.py", "").node/kind file }
#- { File childof Pkg }
#- { File.text _ }  // The contents of this file
#- { File.text/encoding "utf-8" }


def false():
    """Always returns False.

    Used to defeat pykythe's if-then-else evaluations.
    """
    return False


class CC: pass


def testLhsTrailer():
    #- { @i defines/binding TestLhsTrailer_i }
    i = 'abc'
    #- { @d defines/binding TestLhsTrailer_d }
    d = {}
    d[i] = 1
    #- { @d ref TestLhsTrailer_d }
    #- { @i ref TestLhsTrailer_i }
    #- // The following isn't actually wrong but seems weird:
    #- { @x defines/binding vname("${BUILTINS_FQN}.builtins.int.x", _, _, "", python) }
    d[i].x = 2

    e = {}
    e[i] = CC()
    #- { @xx defines/binding vname("${ROOT_FQN}.test_data.bindings.CC.xx", _, _, "", python) }
    e[i].xx = 'xyz'
    #- { @capitalize ref vname("${BUILTINS_FQN}.builtins.str.capitalize", _, _, "", python) }
    e[i].xx.capitalize()

    cond = False
    # TODO: add tests for '.', f(i).x = ...
    if cond:  # Test forward global ref
        #- @testDictFor ref TestDictFor
        testDictFor()

        #- @testDictFor ref TestDictFor
        testDictFor  # Note: no call (for testing when call fails)


if false():
    #- @testDictFor ref TestDictFor  // This is a forward reference test; it would fail in reality.
    testDictFor()


#- { @testDictFor defines/binding TestDictFor }
def testDictFor():
    #- { @x defines/binding TestDictForLocalX=vname("${ROOT_FQN}.test_data.bindings.testDictFor.<local>.x", _, _, "", python) }
    #- { TestDictForLocalX.node/kind variable }
    x = 100
    #- { @#3x ref TestDictForLocalX }
    #- { @#2x defines/binding TestDictFor_for1_x }
    #- { TestDictFor_for1_x.node/kind variable }
    #- { @#0x ref TestDictFor_for1_x }
    #- { @#1x ref TestDictFor_for1_x }
    #- { @#4x ref TestDictFor_for1_x }
    y = {x: x + 1 for x in [1, 2, 3, x] if x % 2 == 0}
    #    0  1         2              3     4
    #- { @#0x ref TestDictForLocalX }
    #- { @#1x ref TestDictForLocalX }
    #- !{ @#0x ref TestDictFor_for1_x }
    #- !{ @#1x ref TestDictFor_for1_x }
    assert x == 100, x
    assert y == {2: 3, 100: 101}, y

#- { @testListFor defines/binding TestListFor }
def testListFor():
    #- { @x defines/binding TestListForLocalX }
    #- { TestListForLocalX.node/kind variable }
    x = 100
    #- { @#2x ref TestListForLocalX }
    #- { @#1x defines/binding TestListForListForX }
    #- { TestListForListForX.node/kind variable }
    #- { @#0x ref TestListForListForX }
    #- { @#3x ref TestListForListForX }
    y = [x + 1 for x in [1, 2, 3, x] if x % 2 == 0]
    #    0         1              2     3
    #- { @x ref TestListForLocalX }
    #- !{ @x ref TestListForListForX }
    assert x == 100
    assert y == [3, 101]
    if false():  # Test global recursive ref
        #- @testListFor ref TestListFor
        testListFor()
    # else:
    #     #- @testListFor ref TestListFor
    #     testListFor()
    nums = [1, 2, 3, 4, 5]
    strs = ['Apple', 'Banana', 'Coconut']
    #- @#0i ref TestListFor_for2_i
    #- @#1i defines/binding TestListFor_for2_i
    #- @#0s ref TestListFor_for2_s
    #- @#2s defines/binding TestListFor_for2_s
    z = [(i, s) for i in nums for s in strs]
    assert z == [(1, 'Apple'), (1, 'Banana'), (1, 'Coconut'), (2, 'Apple'),
                 (2, 'Banana'), (2, 'Coconut'), (3, 'Apple'), (3, 'Banana'),
                 (3, 'Coconut'), (4, 'Apple'), (4, 'Banana'), (4, 'Coconut'),
                 (5, 'Apple'), (5, 'Banana'), (5, 'Coconut')]
    #- {@x defines/binding TestListForLocalX }
    x = 5
    #- { @#1x defines/binding TestListFor_for3_x }
    #- { @#1y defines/binding TestListFor_for3_y }
    #- { @#1z defines/binding TestListFor_for3_z }
    #- { @#0x ref TestListFor_for3_x }
    #- { @#3x ref TestListFor_for3_x }
    #- { @#4x ref TestListFor_for3_x }
    #- { @#5x ref TestListFor_for3_x }
    #- { @#6x ref TestListFor_for3_x }
    #- { @#0y ref TestListFor_for3_y }
    #- { @#2y ref TestListFor_for3_y }
    #- { @#0z ref TestListFor_for3_z }
    #- { @#1z defines/binding TestListFor_for3_z }
    aa = [(x, y, z) for x in range(x) if x % 2 == 0 for y in range(x) if x % 2 == 0 if y % 2 == 0 for z in [x]]
    # x    0            1          2     3                         4     5                                  6
    # y       0                                         1                              2
    # z          0                                                                                    1
    assert aa == [(2, 0, 2), (4, 0, 4), (4, 2, 4)]


def testGexpFor():
    #- { @x defines/binding TestGexpLocalX }
    #- { TestGexpLocalX.node/kind variable }
    x = 100
    #- { @#1x defines/binding TestGexpForX }
    #- { @#0x ref TestGexpForX }
    #- { @#2x ref TestGexpLocalX }
    #- { @#3x ref TestGexpForX }
    #- { @y defines/binding TestGexp_y }
    #- @foo ref Foo
    #- { TestGexpForX.node/kind variable }
    y = foo(0, (x + 1 for x in [1, 2, 3, x] if x % 2 == 0))
    #           0         1              2     3
    #- { @x ref TestGexpLocalX }
    #- !{ @x ref TestGexpForX }
    assert x == 100
    #- @y ref TestGexp_y
    assert list(y) == [4, 102]


def testForLoop():
    #- { @for_range defines/binding TestForLoopForRange }
    for_range = [1, 2, 3]
    #- { @x defines/binding TestForLoopX=vname("${ROOT_FQN}.test_data.bindings.testForLoop.<local>.x", _, _, "", python) }
    #- { @i_num defines/binding TestForLoopINum }
    #- @for_range ref TestForLoopForRange
    for i_num, x in enumerate(for_range):
        #- @i_num ref TestForLoopINum
        #- @i_num ref TestForLoopINum
        #- @x ref TestForLoopX
        foo(i_num, x)
    else:
        #- @foo ref Foo
        #- @i_num ref TestForLoopINum
        #- @#1x ref TestForLoopX
        #- // @#0x ... function param ref foo.<local>.x
        foo(i_num, x=x)
    #- @foo ref Foo
    #- @i_num ref TestForLoopINum
    #- @x ref TestForLoopX
    foo(i_num, x)


def testNonLocal():
    #- { @xxx defines/binding TestNonLocalXxx }
    xxx = 0

    def f():
        #- { @xxx ref TestNonLocalXxx }
        nonlocal xxx
        #- { @xxx defines/binding TestNonLocalXxx }
        xxx = 2
        #- { @xxx ref TestNonLocalXxx }
        return xxx


def testAnnAssign():
    #- { @ee defines/binding TestAnnAssignEe }
    #- { @int ref Int? } // DO NOT SUBMIT: "${ROOT_FQN}.test_data.bindings.int" =>should be "${BUILTINS_FQN}.builtins.int"
    ee: int = 0

    #- @ee ref TestAnnAssignEe
    print(ee)

    #- { @ee2 defines/binding TestAnnAssignEe2 }
    #- { @int ref Int }
    ee2: int

    #- { @str ref STR_0? }  // DO NOT SUBMIT
    #- { @zz1 defines/binding ZZ1? }  // DO NOT SUBMIT
    #- { ZZ1./pykythe/type ZZ1_TYPE? }  // DO NOT SUBMIT - type entry is another file
    zz1 = str()
    #- { @capitalize ref vname("${BUILTINS_FQN}.builtins.str.capitalize", _, _, "", python) }
    zz1.capitalize()

    #- { @int ref vname("${BUILTINS_FQN}.builtins.int", _, _, "", python) }
    #- { @zz2 defines/binding ZZ2? }  // DO NOT SUBMIT
    #- { ZZ2./pykythe/type ZZ2_TYPE? }   // DO NOT SUBMIT - gets "[]"????
    zz2 = int()

    #- @ee2 ref TestAnnAssignEe2
    print(ee2)

    # TODO: ee3  # type: int
    # TODO: ee4 = 0  # type: int


# TODO: This is only a partial test; make a new test that is more
#       thorough, in a separate file
class C:
    def __init__(self):
        self.f1 = [{'a': 1, 'b': 2}, {'c': 3}]

class D(C):
    def __init__(self):
        super().__init__()
        self.f2 = 1

    #- { @m1 defines/binding D_m1 }
    def m1(self):
        return self.f1

    #- { @mx defines/binding D_mx }
    def mx(self):
        return self

class E:
    def __init__(self):
        #- { @f1 defines/binding F1=vname("${ROOT_FQN}.test_data.bindings.E.f1", _, _, "", python) }
        #- { F1./pykythe/type "[class_type('.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.bindings.D',[[class_type('.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.bindings.C',[])]])]" }
        self.f1 = D()

def testTrailers():
    d = D()
    d.f2
    d.f1
    #- { @m1 ref D_m1 }
    d.m1()
    #- { @#0mx ref D_mx }  // because d is class D
    #- // { @#1mx ref D_mx }  // because class D is only one with attr 'mx' DO NOT SUBMIT
    #- // { @m1 ref D_m1 } // ditto DO NOT SUBMIT
    d.mx().mx().m1()
    d.f1[0]['a']
    e.f1.f1


#- { @foo defines/binding Foo }
#- { @x defines/binding FooParamX }
#- { @i defines/binding FooParamI }
def foo(i, x):
    #- { @#0i ref FooParamI }
    #- { @#1x defines/binding FooLocalX }
    #- { @#0x ref FooLocalX }
    #- { @#2x ref FooParamX }
    #- { @#3x ref FooLocalX }
    return (i + x + 1 for x in x if x % 2 == 1)
    #           0         1    2    3


#- @testDictFor ref TestDictFor
testDictFor()
#- @testListFor ref TestListFor
testListFor()
testGexpFor()


def test_global_import():
    #- // TODO: should the following be 'ref' or 'defines/binding'?
    #- { @foo_sep ref G_foo_sep=vname("${ROOT_FQN}.test_data.bindings.foo_sep", _, _, "", python) }
    global foo_sep
    #- { @foo_sep defines/binding G_foo_sep }
    #- { @foo_sep ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path.sep", _, _, "", python) }
    from os.path import sep as foo_sep

    #- // TODO: should the following be 'ref' or 'defines/binding'?
    #- { @foo_sep2 ref G_foo_sep2=vname("${ROOT_FQN}.test_data.bindings.foo_sep2", _, _, "", python) }
    global foo_sep2
    #- { @foo_sep2 defines/binding G_foo_sep2 }
    #- { @foo_sep2 ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path.sep", _, _, "", python) }
    import os.path.sep as foo_sep2
