# DO NOT SUBMIT -- this is a counterpart to t5.py, with "import os.path.sep"

#- { @os defines/binding OS?=vname("${ROOT_FQN}.test_data.t5a.os", _, _, "", python) }
#- { @os ref/imports OS_imports?=vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @path ref/imports OS_PATH_imports?=vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
import os.path

#- { @os ref OS }
#- { @path ref OS_PATH?=vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @sep ref OS_PATH_SEP?=vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
print(os.path.sep)

# import os # DO NOT SUBMIT: delete
# import os.path.sep as os_path_sep # DO NOT SUBMIT: delete
