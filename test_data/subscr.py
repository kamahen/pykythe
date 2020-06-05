# Test subscripting and for loop
 #  - uses classes to test these, so also tests attribute resolution
# TODO: test List[...] parameters and declarations

class C:
    def __init__(self):
        #- { @x defines/binding C_x }
        self.x = 1

# Repeat the definition, so that we can tell when the attributes are
# found by lookup and when they're found by looking at all possibilities.
# e.g., this message in the log:
#   INFO: attr 'x' (ref) 3 possible modules/classes in '/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/subscr.py' @1571-1572: ['.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.subscr.C','.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.subscr.C2','.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.subscr.C3']

class C2:
    def __init__(self):
        self.x = 1
        self.y = 'abc'

class C3:
    def __init__(self):
        self.x = 1


def t1() -> None:

    c = C()
    print(c.x)

    print(C2().x)

    d = [c]
    d0 = d[0]
    print(d0.x)
    print(d[0].x)
    # DO NOT SUBMIT: src_browser:kythe_node(A, '/kythe/node/kind', 'anchor'),
    #                A = vname(_,_,_, F, _), F='tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/subscr.py',
    #                src_browser:kythe_edge(A, '/kythe/edge/tagged', D),
    #                src_browser:kythe_node(D, '/kythe/node/kind', 'diagnostic'),
    #                src_browser:kythe_node(D, '/kythe/message', M),
    #                src_browser:kythe_node(D, '/kythe/details', Details).

    #- { @e defines/binding E_=vname("${ROOT_FQN}.test_data.subscr.t1.<local>.e", _, _, "", python) }
    #- { E_./pykythe/type "[class_type('${ROOT_FQN}.test_data.subscr.C',[])]"}
    for e in d:
        #- { @x ref C_x }
        #- !{ @x tagged _ }
        print(e.x)


def t1a(dd) -> None: # D has unknown type
    for e in dd:
        #- { @x tagged Diagnostic? }
        #- { Diagnostic.node/kind diagnostic }
        #- { Diagnostic.message Message? }
        #- { Diagnostic.details Details? }
        print(d.x)


def t2() -> None:

    print(d.y)     # an error
    print(d[0].y)  # an error

    print(d.z)     # an error
    print(d[0].z)  # an error


t1()
