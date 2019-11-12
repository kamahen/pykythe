# This is an empty dummy file, used for unit tests in pykytye.pl
# The actual contents are provided by the unit tests; but the
# tests need the name of a file that exists so that absolute_file_name
# works properly.

#- { @sep defines/binding vname("${ROOT_FQN}.test_data.dummy_dir.dummy_file.sep", _, _, "", python) }
#- { @#0os    ref/imports vname("${TYPESHED_FQN}.stdlib.3.os",                    _, _, "", python) }
#- { @#0path  ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path",               _, _, "", python) }
#- { @#0sep   ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path.sep",           _, _, "", python) }
from os.path import sep

#- { @dummy_dir   ref/imports vname("${ROOT_FQN}.test_data.dummy_dir", _, _, "", python) }
#- { @dummy_file2 ref/imports vname("${ROOT_FQN}.test_data.dummy_dir.dummy_file2", _, _, "", python) }
#- { @dummy_file2 defines/binding vname("${ROOT_FQN}.test_data.dummy_dir.dummy_file.dummy_file2", _, _, "", python) }
from ..dummy_dir import dummy_file2

#- { @"."            ref/imports vname("${ROOT_FQN}.test_data.dummy_dir", _, _, "", python) }
#- { @#0dummy_file2  ref/imports vname("${ROOT_FQN}.test_data.dummy_dir.dummy_file2", _, _, "", python) }
#- { @dummy_file2a   defines/binding vname("${ROOT_FQN}.test_data.dummy_dir.dummy_file.dummy_file2a", _, _, "", python) }
from . import dummy_file2 as dummy_file2a
