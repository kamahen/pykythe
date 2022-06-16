# DO NOT SUBMIT
# from imports1.py that failed with <unknown> file

# from i1.py that got a wrong type:
#- { @#1III defines/binding III }
#- { @#0III ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III", _, _, "", python) }
#- { @#1III ref/imports III_imports_1? }  // DO NOT SUBMIT
#- { III./pykythe/type III_type_1? }  // "[class_type('${ROOT_FQN}.test_data.imports_dir1.i1_sub.i4.III',[])]" }  // DO NOT SUBMIT
from pykythe.test_data.imports_dir1.i1_sub.i4 import III as III

#- // TODO: verify that __init__, __repr__ resolve properly (__init__ is explicit, __repr__ is implicit, via inheritance)
#- { @III ref III? }
#- { @__init__ ref III_INIT? }
y1 = III.__init__
#- { @III ref III? }
#- { @__repr__ ref III_REPR? }
#- { @encode ref III_ENCODE? }
y2 = III.__repr__.encode()

#- // TODO: the following don't work because "foo" isn't defined; need
#- //       to make undefined items into "object" and it should work.
#- { @foo ref FOO? }
#- // { @__repr__ ref FOO_REPR? }  # DO NOT SUBMIT - should be in object.__repr__
#- // { @encode ref FOO_ENCODE? }  # DO NOT SUBMIT
#- { FOO./pykythe/type _FOO_TYPE }
#- // DO NOT SUBMIT - ensure the result isn't "guessed"
x = foo.__repr__.encode()


#- // TODO: Make the following work
#- // { @#0pykythe defines/binding vname("${ROOT_FQN}.test_data.t8.pykythe", _, _, "", python) }
#- // { @#0pykythe ref/imports PPP? }  // vname("${ROOT_FQN}", _, _, "", python) }  // DO NOT SUBMIT
#- // { @#0test_data ref/imports vname("${ROOT_FQN}.test_data", _, _, "", python) }
#- // { @#0imports_dir1 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1", _, _, "", python) }
#- // { @i6 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i6", _, _, "", python) }
import pykythe.test_data.imports_dir1.i6

#- // { @#0pykythe ref/imports vname("${ROOT_FQN}", _, _, "", python) }
#- // { @#0test_data ref/imports vname("${ROOT_FQN}.test_data", _, _, "", python) }
#- // { @#0imports_dir1 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1", _, _, "", python) }
#- // { @i7 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i7", _, _, "", python) }
#- // { @import_7 ref/imports vname("${ROOT_FQN}.test_data.imports_dir1.i7", _, _, "", python) }
#- // { @import_7 defines/binding Imports1_import_7? }
import pykythe.test_data.imports_dir1.i7 as import_7

#- // { @import7 ref IMPORT7? }
#- // { IMPORT7./pykythe/type IMPORT7_type? }
#- // { @loc ref LOC? }
# #  { LOC./pykythe/type @LOC_type? }
print(import7.loc)
