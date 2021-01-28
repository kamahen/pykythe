# Test that a file without '\n' at end works.

# (There used to be a bug in lib2to3 for inputs without a final '\n')

from os import path

# { @sep ref vname("${TYPESHED_FQN}.stdlib.os.path.sep", _, _, "", python) }
print(path.sep)
