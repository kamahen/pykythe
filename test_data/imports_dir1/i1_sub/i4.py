# TODO: DELETE the following when they're in another test file
#- { @i5    ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i5", _, _, "", python) }
#- { @#1"." ref/imports vname("${ROOT_FQN}.test_data.imports_dir1", _, _, "", python) }
#- { @#0"." ref/imports vname("${ROOT_FQN}.test_data", _, _, "", python) }
from .. import i5

#- { @#0"."   ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub", _, _, "", python) }
#- { @#0i4a   ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4a", _, _, "", python) }
#- { @#0loc   ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4a.loc", _, _, "", python) }
#- { @i4a_loc ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4a.loc", _, _, "", python) }
from .i4a import loc as i4a_loc

assert i4a_loc == "pykythe/test_data/imports_dir/i1_sub/i4a.py"

loc = "pykythe/test_data/imports_dir/i1_sub/i4.py"

class III:
    def __init__(self):
        self.x = loc

print("Entering " + loc + " ... " + __file__)

# TODO: there should also be ref/imports for pykythe, pykythe.test_data, pykythe.test_data.imports_dir1
#- { @pykythe defines/binding vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.pykythe", _, _, "", python) }
#- { @i5 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i5", _, _, "", python) }
import pykythe.test_data.imports_dir1.i5

#- { @i8_i9 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i8.i9", _, _, "", python) }
#- { @i8_i9 defines/binding I8_i9 }
import pykythe.test_data.imports_dir1.i8.i9 as i8_i9

#- { @i5 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i5", _, _, "", python) }
from .. import i5

#- { @#0"."  ref/imports vname("${ROOT_FQN}.test_data",                        _, _, "", python) }
#- { @#1"."  ref/imports vname("${ROOT_FQN}.test_data.imports_dir1",           _, _, "", python) }
#- { @#0i8   ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i8",        _, _, "", python) }
#- { @#0i9   ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i8.i9",     _, _, "", python) }
#- { @#0loc  ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i8.i9.loc", _, _, "", python) }
#- { @i9_loc ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i8.i9.loc", _, _, "", python) }
#- { @i9_loc defines/binding _ }
from ..i8.i9 import loc as i9_loc

assert i5.loc == "pykythe/test_data/imports_dir/i5.py"
assert i9_loc == "pykythe/test_data/imports_dir/i8/i9.py"
assert pykythe.test_data.imports_dir1.i5.loc == i5.loc
#- { @i8_i9 ref I8_i9 }
assert i8_i9.loc == i9_loc
