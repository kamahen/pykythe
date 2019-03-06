# TODO: delete this (much of it is from other files, for debugging)

#- { @int ref INT? } // TODO: should be '${TYPESHED_FQN}.stdlib.2and3.builtins.int', not '${ROOT_FQN}.test_data.t0.int'
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

o#- { @Sized ref Sized }
#- { @xxrange defines/binding Xxrange }
#- { Xxrange./pykythe/type "[class_type('${ROOT_FQN}.test_data.t0.xxrange',[[module_type(module_and_token('${TYPESHED_FQN}.stdlib.3.typing','${TYPESHED_DIR}/stdlib/3/typing.pyi','Sized'))]])]" }
class xxrange(Sized): pass

#- { @sep defines/binding SEP }
from os.path import sep

#- { @os ref/imports vname("${TYPESHED_FQN}.stdlib.3.os", _, _, "", python) }
import os

#- { @print ref PRINT? } // TODO: should be builtins
#- { PRINT./pykythe/type PRINT_type? } // "[func_type('${TYPESHED_FQN}.stdlib.2and3.builtins.print',[])]" }
#- { @os ref OS? }
#- { @path ref vname("${TYPESHED_FQN}.stdlib.3.os.path", _, _, "", python) }
#- { @sep ref vname("${TYPESHED_FQN}.stdlib.3.os.path.sep", _, _, "", python) }
#- // { OS_PATH_SEP./pykythe/type SEP_type? } // TODO
#- { @strip ref OS_PATH_SEP_STRIP? }
#- // { @capitalize ref OS_PATH_SEP_STRIP_CAPITALIZE? } // TODO
print(os.path.sep.strip().capitalize())

#- { @print ref PRINT }
#- { @sep ref SEP }
#- { SEP./pykythe/type SEP_type? } // TODO -- doesn't deref enough
#- { @strip ref SEP_STRIP? }
#- // { @capitalize ref SEP_STRIP_CAPITALIZE? }  // TODO
print(sep.strip().capitalize())
