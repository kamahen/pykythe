# TODO: delete this (much of it is from other files, for debugging)

# Mostly this is tests of '.' binding

#- { @ii defines/binding II }
#- { II./pykythe/type "[import_ref_type(int,'.tmp.pykythe_test.SUBST.BUILTINS.builtins.int',class_type('.tmp.pykythe_test.SUBST.BUILTINS.builtins.int',[]))]" }
#- { @int ref INT? }  // TODO: should be '${BUILTINS_FQN}.builtins.int'
#- // { INT./pykythe/type INT_type? } //  "[class_type('${BUILTINS_FQN}.builtins.int',[])]" } // type info is in another file
ii: int

#- { @iii defines/binding III }
#- { III./pykythe/type "[class_type('${BUILTINS_FQN}.builtins.int',[])]" }
iii = 0

# TODO: validate that the following is *exactly* what typeshed has, with all the conditional executions:
#- { @ABC defines/binding ABC }
#- { ABC./pykythe/type "[class_type('${BUILTINS_FQN}.builtins.str',[])]" }
ABC = " abc "

#- { @ABC ref ABC }
#- { @strip ref _ABC_strip=vname("${BUILTINS_FQN}.builtins.str.strip", _, _, "", python) }
#- // { ABC_strip./pykythe/type ABC_strip_type? } // TODO
#- { @capitalize ref _ABC_strip_capitalize=vname("${BUILTINS_FQN}.builtins.str.capitalize", _, _, "", python) }
#- // { _ABC_strip_capitalize./pykythe/type ABC_strip_capitalize_type? } // TODO
ABC.strip().capitalize()


# TODO: py3_test_grammar doesn't have assignment unpacking to a "*" (PEP 3132)
#- { @xa defines/binding XA? }
#- // { XA./pykythe/type XA_type? }  // TODO: should be there
#- { @xb defines/binding XB? }  // TODO: should get a list type
#- // { XB./pykythe/type XB_type? }  // TODO: should be there
xa, *xb = [1,2,3,4]

#- { @A defines/binding A_class }
#- { A_class.node/kind record }
class A:
    #- { @b defines/binding A_b }
    b: B

#- { @B defines/binding B_class }
class B:
    def __init__(self):
        #- { @#0x defines/binding B_x }
        self.x = [0]  # TODO: also test self.x = {1: 'b'}

#- { @C defines/binding C_class }
class C:
    #- { @a defines/binding C_a }
    #- { C_a.node/kind variable }
    #- { C_a?./pykythe/type C_a_type? }
    a: A

#- { @#0d defines/binding D }
d = []  # TODO: also test d = {}

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

#- { @#0c ref C }
#- { @#0a ref C_a }
#- { @#0b ref A_b }
#- { @#0x defines/binding B_x }
#- { B_x./pykythe/type B_x_type? }
c.a.b.x = []  # TODO: also test self.x = {2: 'x'}

#- { @d ref D }
#- { D./pykythe/type D_type? }
d[111] = [1]

# { @c ref C }
# { @a ref C_a }
# { @b ref A_ b }
# { @x ref B_x }
c.a.b.x[1] = 'abc'

c_list = [c]

c_one = c_list[5]

#- { @a ref C_a }
c_one.a

#- { @a ref C_a }
c_list[5].a
