# TODO: delete this (much of it is from other files, for debugging)

#- { @int ref vname("${BUILTINS_FQN}.builtins.int", _, _, "", python) }
#- { @str ref vname("${BUILTINS_FQN}.builtins.str", _, _, "", python) }
foo(int, str)

aaa = AAA()

#- { @bbb tagged _ }
#- { @ccc tagged _ }
#- // { @ddd tagged _ } // TODO: should this also get a diagnostic? - currently it gets nothing (not even an anchor)
#- //    src_browser query (assuming debug flavour of signatures):
#- //       Path='tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t0.py', V=vname(Sig, Corpus,Root,Path,Lang), src_browser:kythe_node(V, '/kythe/node/kind', anchor), atom_concat(_, '<ddd>', Sig).
aaa.bbb.ccc.ddd = 999
xxx = 1
yyy = [2,3]
yyy[44] = 55
#- { @True ref vname("${BUILTINS_FQN}.builtins.True", _, _, "", python) }
zzz = True

xxx = 'abc'
xxx = 1.23
xxx = 4j
xxx_yyy = xxx

import sys

if sys.version_info >= (3,):
    #- { @Sized defines/binding Sized }
    #- { @Sized ref/imports vname("${TYPESHED_FQN}.stdlib.typing.Sized", _, _, "", python) }
    from typing import Sized

if sysx.version_info >= (3,):
    #- { @Sized ref Sized }
    print(Sized)
else:
    #- { @Sized ref Sized }
    print(Sized)

#- { @Sized ref Sized }
#- { @xxrange defines/binding Xxrange }
#- { Xxrange./pykythe/type "[class_type('${ROOT_FQN}.test_data.t0.xxrange',[])]" }
class xxrange(Sized): pass

#- { @sep defines/binding SEP }
from os.path import sep

#- { @os defines/binding OS }
#- { @os ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
import os

#- { @pow ref vname("${BUILTINS_FQN}.builtins.pow", _, _, "", python) }
#- // { POW./pykythe/type POW_type? } // Type is in another file
pow(1, 2)

#- { @print ref PRINT=vname("${BUILTINS_FQN}.builtins.print", _, _, "", python) }
#- // { PRINT./pykythe/type PRINT_type? } // "[function_type('${BUILTINS_FQN}.builtins.print',[[],[],[],[],[]],[])]" } // TODO: should show keywords  // info is in another file
#- { @os ref OS }
#- { @path ref vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @sep ref vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
#- // { OS_PATH_SEP./pykythe/type SEP_type? } // TODO - in other file
#- { @strip ref vname("${BUILTINS_FQN}.builtins.bytearray.strip", _, _, "", python) }
#- { @encode ref OS_PATH_SEP_STRIP_ENCODE? } // DO NOT SUBMIT - see also os_path_sep1.py
print(os.path.sep.strip().encode())

#- { @print ref PRINT }
#- { @sep ref SEP }
#- // { SEP./pykythe/type SEP_type? } // TODO -- in other file
#- { @strip ref SEP_STRIP? }
#- // { @encode ref SEP_STRIP_ENCODE? }  // TODO
#- // DO NOT SUBMIT - ensure the result isn't "guessed" - see also os_path_sep1.py
print(sep.strip().encode())
