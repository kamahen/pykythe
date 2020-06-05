# DO NOT SUBMIT

#- { @x defines/binding X_1 }
#- { X_1./pykythe/type X_1_type? } // "[class_type('${BUILTINS_FQN}.builtins.bytes',[])]" } // DO NOT SUBMIT
x = b"def"

#- { @decode ref vname("${BUILTINS_FQN}.builtins.bytes.decode", _, _, "", python) }
#- // DO NOT SUBMIT - ensure the result isn't "guessed"
x.decode()


class SomeClass:
    pass

#- { @printx defines/binding PRINTX }
#- { PRINTX./pykythe/type PRINTX_type? }
def printx(*values: object, sep: Text = ..., end: Text = ..., file: Optional[_Writer] = ..., flush: bool = ...) -> None: ...

#- { @print ref PRINT? }
#- // { PRINT./pykythe/type PRINT_type? } // type is in another file
print('foo')

#- { @None ref vname("${BUILTINS_FQN}.builtins.None", _, _, "", python) }
#- // { NONE_0./pykythe/type NONE_0_type? }  // type is in another file
#- { @none defines/binding NONE_1? }
#- { NONE_1./pykythe/type NONE_1_type? }  // type is in another file
none = None

#- { @"__bool__" ref NONE_BOOL_1? }
none.__bool__

#- { @aaa defines/binding AAA_1? }
#- { AAA_1./pykythe/type AAA_1_type? }
#- { @int ref AAA_1_int? }
#- // { AAA_1_int./pykythe/type AAA_1_int_type? }  // type is in another file
aaa: int = 1

def foo():
    #- // { @str ref STR_0? }  // DO NOT SUBMIT
    #- { @zz1 defines/binding ZZ1? }  // DO NOT SUBMIT
    #- { ZZ1./pykythe/type ZZ1_TYPE? }  // DO NOT SUBMIT
    zz1 = 'abc'

    #- { @str ref STR_2? }  // DO NOT SUBMIT
    #- { @zz2 defines/binding ZZ2? }  // DO NOT SUBMIT
    #- { ZZ2./pykythe/type ZZ2_TYPE? }  // DO NOT SUBMIT
    zz2: str

    #- { @str ref STR_3? }  // DO NOT SUBMIT
    #- { @zz3 defines/binding ZZ3? }  // DO NOT SUBMIT
    #- { ZZ3./pykythe/type ZZ3_TYPE? }  // DO NOT SUBMIT
    zz3: str = 'abc'

    #- { @aaa ref AAA_2? }
    #- { AAA_2./pykythe/type AAA_2_type? }
    #- { @zzz5 defines/binding ZZZ5? }
    #- { ZZZ5./pykythe/type ZZZ5_type? }
    zzz5 = aaa

    #- { @zzz6 defines/binding ZZZ6 }
    #- { ZZZ6./pykythe/type ZZZ6_type? }
    #- { @int ref INT_6? }
    zzz6: int
