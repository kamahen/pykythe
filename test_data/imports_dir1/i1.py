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
#- { III./pykythe/type "[class_type('.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.imports_dir1.i1_sub.i4.III',[])]" }
from pykythe.test_data.imports_dir1.i1_sub.i4 import III

assert i4 == i4_b

#- { @loc ref vname("${ROOT_FQN}.test_data.imports_dir1.i3.loc", _, _, "", python) }
assert i3.loc == "pykythe/test_data/imports_dir/i3.py"
assert i3_b.loc == "pykythe/test_data/imports_dir/i3.py"

assert i4.loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"
assert i4_b.loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"
assert i4_loc == "pykythe/test_data/imports_dir/i1_sub/i4.py"

#- { @III ref III }
#- { @iii defines/binding III_global }
#- { III./pykythe/type "[class_type('${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III',[])]" }
#- { III_global./pykythe/type "[class_type('${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III',[])]" }
iii = III()

#- { @III ref III }
#- { @x ref vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III.x", _, _, "", python) }
assert III().x == "pykythe/test_data/imports_dir/i1_sub/i4.py"

#- { @i4_b ref I4_B }
#- { @III ref _I4_B_III=vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III", _, _, "", python) }
#- { @y ref _I4_B_III_Y=vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III.y", _, _, "", python) }
#- { I4_B./pykythe/type "[module_type(module_alone('${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4','/${ROOT_DIR}/test_data/imports_dir1/i1_sub/i4.py'))]" }
#- // TODO:
#- //   _I4_B_III?./pykythe/type isn't useful because the type information
#- //   is in a different file and/or verifier gets confused.
#- //   A = vname('@3101:3102<y>',_,_,P,_), src_browser:kythe_edge(A,E,B),  P='tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/imports_dir1/i1.py', src_browser:kythe_edge(A, E, A2), src_browser:kythe_node(A2, N,V), N='/pykythe/type'.
#- { _I4_B_III_Y./pykythe/type _I4_B_III_Y_type? } // TODO: Why does this give "[]" - src_browser gives "[class_type('.tmp.pykythe_test.SUBST.BUILTINS.builtins.str',[])]"
#- { _I4_B_III./pykythe/type _I4_B_III_type? } // TODO: Why does this give "[]" - src_browser gives "[class_type('.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.imports_dir1.i1_sub.i4.III',[])]"
#- // location information for the above queries:
#- { @i4_b=vname(_,"CORPUS","ROOT","${ROOT_DIR}/test_data/imports_dir1/i1.py","python").node/kind anchor }
#- { @III=vname(_,"CORPUS","ROOT","${ROOT_DIR}/test_data/imports_dir1/i1.py","python").node/kind anchor }
#- { @y=vname(_,"CORPUS","ROOT","${ROOT_DIR}/test_data/imports_dir1/i1.py","python").node/kind anchor }
i4_b.III.y
