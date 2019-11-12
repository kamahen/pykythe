# DO NOT SUBMIT
# from imports1.py that failed with <unknown> file

# from i1.py that got a wrong type:
#- { @#1III defines/binding III }
#- { @#0III ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III", _, _, "", python) }
#- { @#1III ref/imports III_imports_1? }  // DO NOT SUBMIT
#- { III./pykythe/type III_type_1? }  // "[class_type('${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III',[])]" }  // DO NOT SUBMIT
from pykythe.test_data.imports_dir1.i1_sub.i4 import III as III

#- { @foo ref FOO? }
#- { @__repr__ ref FOO_REPR? }
#- { @capitalize ref FOO_CAPITALIZE? }
#- { FOO./pykythe/type _FOO_TYPE }
x = foo.__repr__.capitalize()


# #- { @#0pykythe defines/binding vname("${ROOT_FQN}.test_data.t8.pykythe", _, _, "", python) }
# #- { @#0pykythe ref/imports PPP? }  // vname("${ROOT_FQN}", _, _, "", python) }  // DO NOT SUBMIT
# #- { @#0test_data ref/imports vname("${ROOT_FQN}.test_data", _, _, "", python) }
# #- { @#0imports_dir1 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1", _, _, "", python) }
# #- { @i6 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i6", _, _, "", python) }
import pykythe.test_data.imports_dir1.i6

# #- { @#0pykythe ref/imports vname("${ROOT_FQN}", _, _, "", python) }
# #- { @#0test_data ref/imports vname("${ROOT_FQN}.test_data", _, _, "", python) }
# #- { @#0imports_dir1 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1", _, _, "", python) }
# #- { @i7 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i7", _, _, "", python) }
# #- { @import_7 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i7", _, _, "", python) }
# #- { @import_7 defines/binding Imports1_import_7? }
import pykythe.test_data.imports_dir1.i7 as import_7

# #- { @import7 ref IMPORT7? }
# #- { IMPORT7./pykythe/type IMPORT7_type? }
# #- { @loc ref LOC? }
# #  { LOC./pykythe/type @LOC_type? }
print(import7.loc)
