#- { @#0"." ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub", _, _, "", python) } // DO NOT SUBMIT
#- { @#0i4  ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4", _, _, "", python) }
#- { @import_i4 defines/binding vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4a.import_i4", _, _, "", python) }
#- { @import_i4 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4", _, _, "", python) }
from . import i4 as import_i4

#- { @#0"." ref/imports vname("${ROOT_FQN}.test_data.imports_dir1",        _, _, "", python) }
#- { @#1"." ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub", _, _, "", python) }
#- { @#0i5  ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i5",     _, _, "", python) }
#- { @import_i5 defines/binding vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4a.import_i5", _, _, "", python) }
#- { @import_i5 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i5", _, _, "", python) }
from .. import i5 as import_i5

# TODO: DELETE this file when tests moved elsewhere

loc = "pykythe/test_data/imports_dir/i1_sub/i4a.py"
