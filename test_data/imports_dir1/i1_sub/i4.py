loc = "pykythe/test_data/imports_dir/i1_sub/i4.py"

print("Entering " + loc + " ... " + __file__)

# TODO: there should also be ref/imports for pykythe, pykythe.test_data, pykythe.test_data.imports_dir1
#- { @pykythe defines/binding vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.pykythe", _, _, "", python) }
#- { @i5 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i5", _, _, "", python) }
#- { @"pykythe.test_data.imports_dir1.i5" ref/file vname("", _, _, "${ROOT_DIR}/test_data/imports_dir1/i5.py", "") }
import pykythe.test_data.imports_dir1.i5

#- { @"pykythe.test_data.imports_dir1.i8.i9" ref/file vname("", _, _, "${ROOT_DIR}/test_data/imports_dir1/i8/i9.py", "") }
#- { @i8_i9 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i8.i9", _, _, "", python) }
#- { @i8_i9 defines/binding I8_i9 }
import pykythe.test_data.imports_dir1.i8.i9 as i8_i9

#- { @".." ref/file vname("", _, _, "${ROOT_DIR}/test_data/imports_dir1", "") }
#- { @i5 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i5", _, _, "", python) }
from .. import i5

#- { @"..i8.i9" ref/file vname("", _, _, "${ROOT_DIR}/test_data/imports_dir1/i8/i9.py", "") }
#- { @i9_loc ref/imports I9_loc_imports? }  // TODO: loc should be test_data.imports_dir.i8.i9.loc -- instead it's /tmp/pykythe_test/SUBST/pykythe/test_data/imports_dir1/i8/i9.py.loc
from ..i8.i9 import loc as i9_loc

assert i5.loc == "pykythe/test_data/imports_dir/i5.py"
assert i9_loc == "pykythe/test_data/imports_dir/i8/i9.py"
assert pykythe.test_data.imports_dir1.i5.loc == i5.loc
#- { @i8_i9 ref I8_i9 }
assert i8_i9.loc == i9_loc
