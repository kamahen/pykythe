# TODO: delete this (much of it is from other files, for debugging)

#- { @int ref INT? } // TODO: should be '${BUILTINS_FQN}.builtins.int', not '${ROOT_FQN}.test_data.t0.int'
#- { @str ref STR? } // TODO: (as above)
foo(int, str)

aaa = AAA()

aaa.bbb.ccc.ddd = 999  # TODO: what should this generate (AAA is undefined)?
xxx = 1
yyy = [2,3]
yyy[44] = 55
#- { @True ref TRUE? }  // TODO: should be builtins
zzz = True

xxx = 'abc'
xxx = 1.23
xxx = 4j

import sys

if sys.version_info >= (3,):
    #- { @Sized defines/binding Sized }
    #- { @Sized ref/imports vname("${TYPESHED_FQN}.stdlib.3.typing.Sized", _, _, "", python) }
    from typing import Sized

if sysx.version_info >= (3,):
    #- { @Sized ref Sized }
    print(Sized)
else:
    #- { @Sized ref Sized }
    print(Sized)

#- { @Sized ref Sized }
#- { @xxrange defines/binding Xxrange }
#- { Xxrange./pykythe/type XXRANGE_type? } // TODO: this gets ${TYPESHED_FQN}.3.typing.Sized instead of ${TYPESHED_FQN}.typeshed.3.typing.Sized
class xxrange(Sized): pass

#- { @sep defines/binding SEP }
from os.path import sep

#- { @os ref/imports vname("${TYPESHED_FQN}.stdlib.3.os", _, _, "", python) }
import os

#- { @pow ref vname("${BUILTINS_FQN}.builtins.pow", _, _, "", python) }
#- // { POW./pykythe/type POW_type? } // Type is in another file
pow(1, 2)

#- { @print ref PRINT=vname("${BUILTINS_FQN}.builtins.print", _, _, "", python) }
#- // { PRINT./pykythe/type PRINT_type? } // "[function_type('${BUILTINS_FQN}.builtins.print',[[],[],[],[],[]],[])]" } // TODO: should show keywords  // info is in another file
#- { @os ref OS? }
#- { @path ref vname("${TYPESHED_FQN}.stdlib.3.os.path", _, _, "", python) }
#- { @sep ref vname("${TYPESHED_FQN}.stdlib.3.os.path.sep", _, _, "", python) }
#- // { OS_PATH_SEP./pykythe/type SEP_type? } // TODO
#- { @strip ref OS_PATH_SEP_STRIP? } // TODO: should be str.strip, not object.strip
#- // { @capitalize ref OS_PATH_SEP_STRIP_CAPITALIZE? } // TODO
print(os.path.sep.strip().capitalize())

#- { @print ref PRINT }
#- { @sep ref SEP }
#- { SEP./pykythe/type SEP_type? } // TODO -- doesn't deref enough
#- { @strip ref SEP_STRIP? }
#- // { @capitalize ref SEP_STRIP_CAPITALIZE? }  // TODO
print(sep.strip().capitalize())
