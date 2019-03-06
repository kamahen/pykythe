#- @loc defines/binding vname("${ROOT_FQN}.test_data.imports_dir1.i1.loc", _, _, "", python)
loc = "pykythe/test_data/imports_dir/i1.py"

print("Entering " + loc + " ... " + __file__)

from . import i3

from pykythe.test_data.imports_dir1 import i3 as i3_b

assert i3 == i3_b

from .i1_sub import i4 as i4

from pykythe.test_data.imports_dir1.i1_sub import i4 as i4_b

from pykythe.test_data.imports_dir1.i1_sub.i4 import loc as i4_loc

#- { @III defines/binding III }
#- { @III ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III", _, _, "", python) }
from pykythe.test_data.imports_dir1.i1_sub.i4 import III

assert i4 == i4_b

#- { @loc ref vname("${ROOT_FQN}.test_data.imports_dir1.i3.loc", _, _, "", python) }
assert i3.loc == "pykythe/test_data/imports_dir/i3.py"
assert i3_b.loc == "pykythe/test_data/imports_dir/i3.py"

assert i4.loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"
assert i4_b.loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"
assert i4_loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"

#- { @III ref III }
#- // { @x ref III_x? }  // TODO
assert III().x == "pykythe/test_data/imports_dir/i1_sub/i4.py"
