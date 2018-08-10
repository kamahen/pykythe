loc = "pykythe/test_data/imports_dir/i1_sub/i4.py"

print("Entering " + loc + " ... " + __file__)

from .. import i5
from ..i8.i9 import loc as i9_loc

assert i5.loc == "pykythe/test_data/imports_dir/i5.py"
assert i9_loc == "pykythe/test_data/imports_dir/i8/i9.py"
