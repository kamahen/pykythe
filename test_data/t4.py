# TODO: delete this

#- { @ABC defines/binding ABC }
#- { ABC./pykythe/type ABC_type? }
ABC = " abc "
#- { @strip ref STRIP? }
#- { @capitalize ref CAPITALIZE? }
#- // { STRIP./pykythe/type STRIP_type? }
#- // { CAPITALIZE./pykythe/type CAPITALIZE_type? }
ABC.strip().capitalize()

class B:
    z = 'zzz'

class C:
    #- { @x defines/binding C_x? }
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

#- { @strip ref STRIP? }
#- { @capitalize ref CAPITALIZE? }
c0.x.strip().capitalize()

from lib2to3.pgen2 import token, pytree

#- { @token ref TOKEN? }
#- { @ENDMARKER ref ENDMARKER? }
print(token.ENDMARKER)

node: pytree.Base
ch0 = node.children[0]
print(node, node.children, ch0, ch0.type, token.LSQB)

if ch0.type == token.LSQB:
    print('LSQB')
