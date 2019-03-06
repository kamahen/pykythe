# TODO: delete this (much of it is from other files, for debugging)

#- { @ii defines/binding II }
#- { II./pykythe/type "[class_type('${TYPESHED_FQN}.stdlib.2and3.builtins.int',[])]" }
#- { @int ref INT? }  // TODO: should be '${TYPESHED_FQN}.stdlib.2and3.builtins.int'
#- { INT./pykythe/type "[class_type('${TYPESHED_FQN}.stdlib.2and3.builtins.int',[])]" }
ii: int

#- { @iii defines/binding III }
#- { III./pykythe/type "[class_type('${TYPESHED_FQN}.stdlib.2and3.builtins.int',[])]" }
iii = 0

# TODO: validate that the following is *exactly* what typeshed has, with all the conditional executions:
#- { @ABC defines/binding ABC }
#- { ABC./pykythe/type "[class_type('${TYPESHED_FQN}.stdlib.2and3.builtins.str',[[class_type('${TYPESHED_FQN}.stdlib.2and3.builtins.basestring',[]),class_type('${TYPESHED_FQN}.stdlib.2and3.builtins.object',[])]])]" }
ABC = " abc "

#- { @ABC ref ABC }
#- { @strip ref vname("${TYPESHED_FQN}.stdlib.2and3.builtins.str.strip", _, _, "", python) }
#- // { ABC_strip./pykythe/type ABC_strip_type? }  // TODO
#- { @capitalize ref vname("${TYPESHED_FQN}.stdlib.2and3.builtins.str.capitalize", _, _, "", python) }
#- // { ABC_capitalize./pykythe/type ABC_strip_capitalize_type? } // TODO
ABC.strip().capitalize()


# TODO: py3_test_grammar doesn't have assignment unpacking to a "*" (PEP 3132)
#- { @xa defines/binding XA? }
#- // { XA./pykythe/type XA_type? } // TODO
#- { @xb defines/binding XB? }  // TODO: should get a list type
#- // { XB./pykythe/type XB_type? } // TODO
xa, *xb = [1,2,3,4]

#- { @A defines/binding A_class }
#- { A_class.node/kind record }
class A:
    #- { @b defines/binding A_b }
    b: B

#- { @B defines/binding B_class }
class B:
    def __init__(self):
        #- { @x defines/binding B_x }
        self.x = {}

#- { @C defines/binding C_class }
class C:
    #- { @a defines/binding C_a }
    #- { C_a.node/kind variable }
    #- { C_a?./pykythe/type C_a_type? }
    a: A

#- { @d defines/binding D }
d = {}

#- { @c defines/binding C }
#- { @C ref C_class }
c = C()

#- { @c ref C }
#- { @a defines/binding C_a }
#- { @A ref A_class }
#- { C?./pykythe/type C_type? }
c.a = A()

#- { @c ref C }
#- { @a ref C_a }
#- { @b defines/binding A_b }
#- { @B ref B_class }
#- { A_b?./pykythe/type A_b_type? }
c.a.b = B()

#- { @c ref C }
#- { @a ref C_a }
#- { @b ref A_b }
#- { @x defines/binding B_x }
#- { B_x./pykythe/type B_x_type? }
c.a.b.x = {}

#- { @d ref D }
d[111] = [1]

# { @c ref C }
# { @a ref C_a }
# { @b ref A_ b }
# { @x ref B_x }
c.a.b.x[1][2] = 'abc'
