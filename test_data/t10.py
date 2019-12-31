
def f00():
    # 0x251c ()
    return format(qqsv('a'), '02x')

def foo():
    # 0x251c (''\u251c'')
    return format(QQSV('├'), '02X')

# xformat(xqqsv('├'), '02x')

#- { @str ref vname("${BUILTINS_FQN}.builtins.str", _, _, "", python) }
aa = str

#- { @str ref vname("${BUILTINS_FQN}.builtins.str", _, _, "", python) }
bb = str()

#- { @str ref vname("${BUILTINS_FQN}.builtins.str", _, _, "", python) }
ee: str # = 'abc'

def foo():
    #- { @str ref vname("${BUILTINS_FQN}.builtins.str", _, _, "", python) }
    ff: str = 'xyz'

foo()

class C:
    def __init__(self):
        self.x = 123

    def set_x(self, x):
        self.x = x

c = C()
print(c.x)

c2 = C()
c2.x = 156

#- { @pow ref POW? }
#- // { POW./pykythe/type POW_type? } // TODO: ".tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.t10.pow"./pykythe/type is set ... should it be?
pow(1, 2)

a = 'a' if ee else 'b'

b = [x for x in ee if x.isprintable()]


#- { @printx ref/imports PRINTX_imports? }
from .t9 import printx

#- { @printx ref PRINTX_var? }
#- { PRINTX_var./pykythe/type PRINTX_type? }
#- { @foo defines/binding FOO? }
#- { FOO./pykythe/type FOO_type? }
foo = printx


from .t9 import SomeClass

#- { @some_class defines/binding SOME_CLASS? }
#- { SOME_CLASS./pykythe/type SOME_CLASS_type? }
some_class = SomeClass
