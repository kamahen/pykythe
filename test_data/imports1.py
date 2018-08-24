"""Imports tests.

Assumes that PYTHONPATH=../..

The verifier variable names assume that all the .py files are given

Test by (when CWD is same directory as this file):
   PYTHONPATH=../.. python3.7 -B imports1.py
or
   cd ../..; PYTHONPATH=. python3.7 -B pykythe/test_data/imports1.py

That is, the "root" directory is one above pykythe (this assumes that
there are other top-level directories, such as typeshed).
"""

#- // This code assumes that it is in $ROOT/pykythe/test_data/imports1.py:
#- { Pkg=vname("${ROOT_FQN}.test_data.imports1", "test-corpus", "test-root", "", python).node/kind package }
#- { File=vname("", "test-corpus", "test-root", "${ROOT_DIR}/test_data/imports1.py", "").node/kind file }
#- { File childof Pkg }

print("Entering imports1.py: " + __file__)

#- { @"pykythe.test_data.imports_dir1" ref/file vname("", "test-corpus", "test-root", "${ROOT_DIR}/test_data/imports_dir1", "") }
#- { @i1 defines/binding Imports1_i1=vname("${ROOT_FQN}.test_data.imports1.i1", _, _, "", python) }
#- { @i1 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1", _, _, "", python) }
from pykythe.test_data.imports_dir1 import (i1,
#- { @i3 defines/binding Import1_i3 }
#- { @i3 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i3", _, _, "", python) }
                                            i3)

#- { @#0i3 ref Import1_i3 }
#- { @loc ref Import1_i3_loc? }  // TODO: vname("${ROOT_FQN}.test_data.imports_dir1.i3.loc, _, _, "", python)
assert i3.loc == "pykythe/test_data/imports_dir/i3.py"

# i2 is a token in imports_file.py
from pykythe.test_data.imports_file1 import i2

import pykythe.test_data.imports_file1

import pykythe.test_data.imports_file1 as i_f1

#- { @#0i1 ref Imports1_i1 }
#- // { @loc ref I1_loc? }  // TODO: vname("${ROOT_FQN}.test_data.imports_dir1.i1.loc, _, _, "", python)
assert i1.loc == "pykythe/test_data/imports_dir/i1.py"

assert i2 == "pykythe/test_data/imports_file1.py"

assert pykythe.test_data.imports_file1.i2 == i2

assert i_f1.i2 == i2

#- { @"pykythe.test_data.imports_dir1.i6" ref/file vname("", _, _, "${ROOT_DIR}/test_data/imports_dir1/i6.py", "") }
#- { @"pykythe.test_data.imports_dir1.i7" ref/file vname("", _, _, "${ROOT_DIR}/test_data/imports_dir1/i7.py", "") }
#- { @import_7 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i7", _, _, "", python) }
#- { @import_7 defines/binding Imports1_import_7 }
#- { @#0pykythe defines/binding vname("${ROOT_FQN}.test_data.imports1.pykythe", _, _, "", python) }
#- // TODO: pykythe.test_data ref/file, etc.
import pykythe.test_data.imports_dir1.i6, pykythe.test_data.imports_dir1.i7 as import_7

#- { @#0pykythe ref vname("${ROOT_FQN}.test_data.imports1.pykythe", _, _, "", python) }
assert pykythe.test_data.imports_dir1.i6.loc == "pykythe/test_data/imports_dir/i6.py"

#- { @import_7 ref Imports1_import_7 }
assert import_7.loc == "pykythe/test_data/imports_dir/i7.py"

from pykythe.test_data.imports_dir1 import i8

assert i8.loc == "pykythe/test_data/imports_dir/i8/__init__.py"

from pykythe.test_data.imports_dir1.i8 import i9

assert i9.loc == "pykythe/test_data/imports_dir/i8/i9.py"

#- { @#0os ref/file vname("", "test-corpus", "test-root", "${TYPESHED_DIR}/stdlib/3/os/__init__.pyi", "") }
#- { @os_path ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path", _, _, "", python) }
#- { @os_path defines/binding OsPath2 }
#- { @os_sep ref/imports vname("${TYPESHED_DIR}/stdlib/3/os/__init__.pyi.sep", _, _, "", python) }
#- { @os_sep defines/binding OsSep? }  // TODO: add test cases for os_path is typeshed/stdlib/3/os/path.pyi and os_sep is typeshed/stdlib/3/os.pyi::sep
from os import path as os_path, sep as os_sep

#- { @#0os_path ref OsPath2 }
#- { @#1os_path ref OsPath2 }
#- { @#2os_path ref OsPath2 }
print("Curdir: " + os_path.curdir + " => " + os_path.abspath(os_path.curdir))
# The following only works if curdir is this directory:
# assert os_path.abspath(os_path.curdir).endswith("pykythe/test_data")

#- { @"os.path" ref/file vname("", _, _, "${TYPESHED_DIR}/stdlib/3/os/path.pyi", "") }
import os.path

assert os_path == os.path

#- // { @os_path ref OsPath? }
#- // { @#0os ref/module XXX3? }  // vname("typeshed.stdlib.3.os", _, _, "", python)
#- // { @#0path ref/module XXX4? }  //  vname("typeshed.stdlib.3.os.path", _, _, "", python)
#- // { @#0curdir ref OsPathCurdir5? }
#- // { @#1curdir ref OsPathCurdir6? }
assert os.path.curdir == os_path.curdir


#- { @#0os ref/file vname("", "test-corpus", "test-root", "${TYPESHED_DIR}/stdlib/3/os/__init__.pyi", "") }
#- // { @#0name ref YYYY? } // vname("${TYPESHED_FQN}.typeshed.stdlib.3.os.name", _, _, "", python) }   // TODO
#- { @os_name defines/binding OsName }
from os import name as os_name

#- { @os defines/binding OS=vname("${ROOT_FQN}.test_data.imports1.os", _, _, "", python) }
#- { @os ref/imports vname("${TYPESHED_FQN}.stdlib.3.os", _, _, "", python) }
import os

#- { @"os.path" ref/file vname("", _, _, "${TYPESHED_DIR}/stdlib/3/os/path.pyi", "") }
#- { @os defines/binding OS=vname("${ROOT_FQN}.test_data.imports1.os", _, _, "", python) }
#- { @path ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path", _, _, "", python) }
import os.path

# import os gets os/__init__.pyi
#- { @#0os ref/file vname("", _, _, "${TYPESHED_DIR}/stdlib/3/os/__init__.pyi", "") }
#- !{ @#0os defines/binding _ }
#- { @my_os ref/imports vname("${TYPESHED_FQN}.stdlib.3.os", _, _, "", python) }
import os as my_os

#- { @#0"os.path" ref/file vname("", _, _, "${TYPESHED_DIR}/stdlib/3/os/path.pyi", "") }
#- // @#0os ref/file ...  // TODO
#- !{ @#0os defines/binding _ }
#- { @os_path ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path", _, _, "", python) }
import os.path as os_path

#- { @#1os ref OS }
#- { @"os_name" ref OsName }
assert os_name == os.name

# import sys gets sys.pyi
#- { @sys defines/binding vname("${ROOT_FQN}.test_data.imports1.sys", _, _, "", python) }
#- { @sys ref/file vname("", _, _, "${TYPESHED_DIR}/stdlib/3/sys.pyi", "") }
#- { @sys ref/imports vname("${TYPESHED_FQN}.stdlib.3.sys", _, _, "", python) }
import sys

#- { @my_sys defines/binding vname("${ROOT_FQN}.test_data.imports1.my_sys", _, _, "", python) }
#- { @#0sys ref/file vname("", _, _, "${TYPESHED_DIR}/stdlib/3/sys.pyi", "") }
#- { @my_sys ref/imports vname("${TYPESHED_FQN}.stdlib.3.sys", _, _, "", python) }
import sys as my_sys

