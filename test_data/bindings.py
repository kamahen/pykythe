"""Exercise various bindings/ref patterns."""

# See comment in py3_test_grammar.py about the "{...}"s.

# TODO: These only test Python 3; need similar tests for Python2
# (e.g., list comprehensions "leak" in PYthon2)


def testLhsTrailer():
    #- { @i defines/binding TestLhsTrailer_i }
    i = 0
    #- { @d defines/binding TestLhsTrailer_d }
    d = {}
    d[i] = 1
    #- { @d ref TestLhsTrailer_d }
    #- { @i ref TestLhsTrailer_i }
    #- // TODO: generates the following, which is incorrect:
    #- // test_data.py3_test_grammar.GrammarTests.testLhsTrailer.<local>.x
    #- // { @x defines/binding TestLhsTrailer_x? }
    d[i].x = 2
    # TODO: add tests for '.', f(i).x = ...
    if False:  # Test forward global ref
        #- @testDictFor ref TestDictFor
        testDictFor()


#- { @testDictFor defines/binding TestDictFor }
def testDictFor():
    #- { @x defines/binding TestDictForLocalX=vname("test_data.bindings.testDictFor.<local>.x", _, _, "", python) }
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
    if False:  # Test global recursive ref
        #- @testListFor ref TestListFor
        testListFor()
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
    y = foo(x + 1 for x in [1, 2, 3, x] if x % 2 == 0)
    #       0         1              2     3
    #- { @x ref TestGexpLocalX }
    #- !{ @x ref TestGexpForX }
    assert x == 100
    #- @y ref TestGexp_y
    assert list(y) == [4, 102]


def testNonLocal():
    #- { @x defines/binding TestNonLocalX }
    x = 0

    def f():
        #- @x ref TestNonLocalX
        nonlocal x
        #- @x ref TestNonLocalX?  // TODO: should be defines/binding
        x = 2
        #- @x ref TestNonLocalX
        return x


#- { @foo defines/binding Foo }
#- { @x defines/binding FooParamX }
def foo(x):
    #- { @#1x defines/binding FooLocalX }
    #- { @#0x ref FooLocalX }
    #- { @#2x ref FooParamX }
    #- { @#3x ref FooLocalX }
    return (x + 1 for x in x if x % 2 == 1)
    #       0         1    2    3


#- @testDictFor ref TestDictFor
testDictFor()
#- @testListFor ref TestListFor
testListFor()
testGexpFor()
