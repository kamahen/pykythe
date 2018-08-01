#- @loc defines/binding I1_loc
loc = "pykythe/test_data/imports_dir/i1.py"

print("Entering " + loc + " ... " + __file__)

from . import i3

from pykythe.test_data.imports_dir1 import i3 as i3_b

assert i3 == i3_b

from .i1_sub import i4 as i4

from pykythe.test_data.imports_dir1.i1_sub import i4 as i4_b

assert i4 == i4_b

assert i3.loc == "pykythe/test_data/imports_dir/i3.py"
assert i3_b.loc == "pykythe/test_data/imports_dir/i3.py"

assert i4.loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"
assert i4_b.loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"
