# TODO: delete this

#- { @ABC defines/binding ABC }
#- { ABC./pykythe/type "[class_type('${BUILTINS_FQN}.builtins.str',[])]" }
ABC = " abc "
#- { @strip ref vname("${BUILTINS_FQN}.builtins.str.strip", _, _, "", python) }
#- { @capitalize ref vname("${BUILTINS_FQN}.builtins.str.capitalize", _, _, "", python) }
#- // The following two /pykythe/type items are in builtins.kythe.json
#- // but we know that they work because strip() and capitalize() ref's are correct:
#- // { STRIP./pykythe/type STRIP_type? }
#- // { CAPITALIZE./pykythe/type CAPITALIZE_type? }
ABC.strip().capitalize()

class B:
    z = 'zzz'

class C:
    #- { @x defines/binding C_x=vname("${ROOT_FQN}.test_data.t4.C.x", _, _, "", python) }
    x = 'abc'
    def b(self) -> B:
        return B()

def makeC() -> C:
    return C()

makeC().b().z

c = makeC()

#- { @x ref C_x }  // TODO: gets object.x; should get C_x
c.x

c0 = C()

#- { @x ref C_x }
c0.x

#- { @strip ref vname("${BUILTINS_FQN}.builtins.str.strip", _, _, "", python) }
#- { @capitalize ref vname("${BUILTINS_FQN}.builtins.str.capitalize", _, _, "", python) }
c0.x.strip().capitalize()

from lib2to3.pgen2 import token
from lib2to3 import pytree

#- { @token ref vname("${ROOT_FQN}.test_data.t4.token", _, _, "", python) }
#- // { @token ref/imports TOKEN? }  // DO NOT SUBMIT
#- { @ENDMARKER ref vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pgen2.token.ENDMARKER", _, _, "", python) }
print(token.ENDMARKER)

#- { @Base ref vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pytree.Base", _, _, "", python) }
node: pytree.Base

#- { @parent ref vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pytree.Base.parent", _, _, "", python) }
#- { @type ref vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pytree.Base.type", _, _, "", python) }
#- { @type ref vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pytree.Base.type", _, _, "", python) }
#- { @real ref vname("${BUILTINS_FQN}.builtins.int.real", _, _, "", python) }
print(node.parent.type.real)

#- { @children ref _CHILDREN=vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pytree.Base.children", _, _, "", python) }
#- { _CHILDREN./pykythe/type "[class_type('${BUILTINS_FQN}.builtins.str',[])]" }
#- { @ch0 defines/binding CH0=vname("${ROOT_FQN}.test_data.t4.ch0", _, _, "", python) }
#- { CH0./pykythe/type CH0_TYPE? } // DO NOT SUBMIT - should not be "[]"
ch0 = node.children[0]
#- // TODO: when we properly handle type inferencing for ch0, make another test
#- //       that has ch0 without a type and which generates the 3 possibilities below
#- { @type ref vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pytree.Base.type", _, _, "", python) }  // TODO: other vname's shouldn't appear, but currently there are 3 alternatives
#- { @type ref vname("${TYPESHED_FQN}.stdlib.2and3.lib2to3.pytree.BasePattern.type", _, _, "", python) }  // TODO: delete
#- { @type ref vname("${TYPESHED_FQN}.stdlib.3._ast.ExceptHandler.type", _, _, "", python) }  // TODO: delete
print(node, node.children, ch0, ch0.type, token.LSQB)

if ch0.type == token.LSQB:
    print('LSQB')
