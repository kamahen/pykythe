# TODO: delete this when "import os.path" works 100%
#       (it's a relatively rare thing, so the fact that it isn't
#       quite right is slightly acceptable).


# TODO: Fix recursive list_of_type:

def foo():
    root = []
    root[:] = [root]


#- { @os ref/imports OS_imports? } // TODO: should be ${TYPESHED_FQN}.stdlib.3.os, not ${TYPESHED_FQN}.stdlib.3.os.path
#- { @path ref/imports vname("${TYPESHED_FQN}.stdlib.3.os.path", _, _, "", python) }
import os.path  # // TODO: fix this bug(#19)

#- { @sep ref SEP? } // =vname("${TYPESHED_FQN}.stdlib.3.os.path.sep", _, _, "", python) }
#- { SEP./pykythe/type SEP_type? }  // TODO: Should be str, not []
#- { @path ref PATH? } // =vname("${TYPESHED_FQN}.stdlib.3.os.path", _, _, "", python) }
#- // { PATH./pykythe/type PATH_type? } // TODO: this gives correct module part but path needs to be cleaned up
#- { @os ref OS?=vname("${ROOT_FQN}.test_data.t5.os", _, _, "", python) }
#- { OS./pykythe/type OS_type? } // "[module_type(module_alone('${TYPESHED_FQN}.stdlib.3.os',''))]" }
#- // TODO: should have @os ref/imports and maybe also @path ref/imports
print(os.path.sep)
