"""For development - short tests.

TODO: delete this file.
"""

# The following is illegal Python but there are circumstances where it
# would be legal (e.g., if sep were assigned to a module).

#- { @os_path_sep defines/binding vname("${ROOT_FQN}.test_data.test0.os_path_sep", _, _, "", python) }
#- { @#0os ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @#0path ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0sep ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
from os.path import sep as os_path_sep

#- { @#0sep defines/binding vname("${ROOT_FQN}.test_data.test0.sep", _, _, "", python) }
#- { @#0os ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @#0path ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0sep ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
from os.path import sep

#- { @os_path_sep defines/binding vname("${ROOT_FQN}.test_data.test0.os_path_sep", _, _, "", python) }
#- { @#0os ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @#0path ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0sep ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
import os.path.sep as os_path_sep

#- { @#0os defines/binding vname("${ROOT_FQN}.test_data.test0.os", _, _, "", python) }
#- { @#0os ref/imports vname("${TYPESHED_FQN}.stdlib.os", _, _, "", python) }
#- { @#0path ref/imports vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @#0sep ref/imports vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
import os.path.sep

import os
#- { @os ref X1? }  // TODO: typeshed os
#- { @path ref vname("${TYPESHED_FQN}.stdlib.os.path", _, _, "", python) }
#- { @sep ref vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
print(os.path.sep)



# !!!
# !!!# TODO: file bug to kythe about this non-ASCII character in a comment
# !!!#       causing verifier to fail if it appears near the top of the
# !!!#       source file [format(ord('├'), '02x') == 0x251c]:
# !!!# ├
