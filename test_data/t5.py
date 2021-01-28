# TODO: delete this when "import os.path" works 100%
#       (it's a relatively rare thing, so the fact that it isn't
#       quite right is slightly acceptable).


# TODO: Fix recursive list_of_type:

def foo():
    root = []
    root[:] = [root]


#- { @#0os ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @#0path ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0sep ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
#- { @os_path_sep defines/binding OS_PATH_SEP_local=vname("${ROOT_FQN}.test_data.t5.os_path_sep", _, _, "", python) }
import os.path.sep as os_path_sep  # // DO NOT SUBMIT: verify fix this bug(#19) https://github.com/kamahen/pykythe/issues/19

#- { @os_path_sep ref OS_PATH_SEP_local }
print(os_path_sep)

#- { @os ref/imports OS=vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @path ref/imports OS_PATH=vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @sep ref/imports OS_PATH_SEP=vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
#- { @os defines/binding OS_local=vname("${ROOT_FQN}.test_data.t5.os", _, _, "", python) }
import os.path.sep  # // DO NOT SUBMIT: verify fix this bug(#19) https://github.com/kamahen/pykythe/issues/19

#- { @os ref OS_local }
#- { @path ref OS_PATH }
#- { @sep ref OS_PATH_SEP }
print(os.path.sep)

#- { @os ref/imports OS }
#- { @os defines/binding OS_local }
import os

