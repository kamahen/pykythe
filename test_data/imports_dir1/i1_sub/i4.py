loc = "pykythe/test_data/imports_dir/i1_sub/i4.py"

print("Entering " + loc + " ... " + __file__)

from .. import i5

assert i5.loc == "pykythe/test_data/imports_dir/i5.py"
