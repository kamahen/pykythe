
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

#- { @pow ref _POW=vname("${BUILTINS_FQN}.builtins.pow", _, _, "", python) }
#- // { _POW./pykythe/type POW_type? } // TODO
pow(1, 2)

a = 'a' if ee else 'b'

b = [x for x in ee if x.isprintable()]


#- { @printx defines/binding PRINTX_var=vname("${ROOT_FQN}.test_data.t10.printx", _, _, "", python) }
#- { @printx ref/imports vname("${ROOT_FQN}.test_data.t9.printx", _, _, "", python) }
from .t9 import printx

#- { @printx ref PRINTX_var }
#- { PRINTX_var./pykythe/type _PRINTX_var_type="[function_type('${ROOT_FQN}.test_data.t9.printx',[[],[],[],[],[]],[import_ref_type('None','${BUILTINS_FQN}.builtins.None',import_ref_type('None','${BUILTINS_FQN}.builtins.None',class_type('${BUILTINS_FQN}.builtins.NoneType',[])))])]" }
#- { @foo defines/binding FOO }
#- { FOO./pykythe/type _PRINTX_var_type_? }  // TODO: should be _PRINTX_var_type
foo = printx


from .t9 import SomeClass

#- { @some_class defines/binding SOME_CLASS }
#- { SOME_CLASS./pykythe/type "[class_type('${ROOT_FQN}.test_data.t9.SomeClass',[])]" }
some_class = SomeClass
