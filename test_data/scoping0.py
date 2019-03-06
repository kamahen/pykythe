# import builtins
# from builtins import *

try:
    #- { @y ref GlobalY=vname("${ROOT_FQN}.test_data.scoping0.y", _, _, "", python) }
    #- { @x defines/binding vname("${ROOT_FQN}.test_data.scoping0.x", _, _, "", python) }
    x = y
except NameError:
    pass

#- { @y defines/binding GlobalY }
y = 1

#- { @x defines/binding ParamX=vname("${ROOT_FQN}.test_data.scoping0.foo.<local>.x", _, _, "", python) }
#- { @int ref Int? }  // TODO: should get builtins.int
def foo(x: int):
    try:
        #- { @x ref ParamX }
        #- { @y ref LocalY=vname("${ROOT_FQN}.test_data.scoping0.foo.<local>.y", _, _, "", python) }
        return x + y
    except NameError:
        pass
    #- { @y defines/binding LocalY }
    y = 1

try:
    print(foo(111))
except NameError:
    pass

