# Some tests for globals (from builtins) and related stuff
# TODO: This is very preliminary; so far just checks that None, True, False are handled properly.

# TODO: Why do the following 3 produce different types?
MyTrue = bool(1)
MyFalse: bool
MyTrueFalse: bool = bool(-1)

#- { @int ref vname("${BUILTINS_FQN}.builtins.int", CORPUS, ROOT, "", python) }
x: int

#- { @None ref vname("${BUILTINS_FQN}.builtins.None", CORPUS, ROOT, "", python) }
None

#- { @True ref vname("${BUILTINS_FQN}.builtins.True", CORPUS, ROOT, "", python) }
True

#- { @False ref vname("${BUILTINS_FQN}.builtins.False", CORPUS, ROOT, "", python) }
False

t = True
f = False
#- { @bool ref vname("${BUILTINS_FQN}.builtins.bool", CORPUS, ROOT, "", python) }
tf = bool(0)

#- { @NoneType ref vname("${BUILTINS_FQN}.builtins.NoneType", CORPUS, ROOT, "", python) }
nn = NoneType


yy = None

print(None)

class C: pass

c: C

c2 = C()

def foo(y: str, z: C) -> None:
    a = C()
    print(x, y, int, a, None)


w = Warning()


# Override a builtin
#- { @complex defines/binding COMPLEX=vname("${ROOT_FQN}.test_data.t14.complex", CORPUS, ROOT, "", python) }
complex = 0

#- { @complex ref COMPLEX }
print(complex(2,1))

#- { @unkn1 ref vname("${ROOT_FQN}.test_data.t14.unkn1", CORPUS, ROOT, "", python) }
unkn1

#- { @unkn2 defines/binding vname("${ROOT_FQN}.test_data.t14.unkn2", CORPUS, ROOT, "", python) }
unkn2 = 2
