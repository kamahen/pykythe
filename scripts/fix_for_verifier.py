#!/usr/bin/env python3.7
"""Read files and do substitutions that make it suitable for Kythe verifier.

Takes three arguments: from_dir to_dir typeshed_dir

Does a copy from from_dir to to_dir, with substitutions.

The 'root path' is the absolute form of one directory up from to_dir
(that is, root_dir is the absolute form of 'to_dir/..'). This
replaces ${ROOT_DIR} in the file. ${ROOT_FQN} gets the same
subsitution, except with '.'s instead of '/s'.

The 'typeshed path' is the absolute form of typeshed_dir.
It uses ${TYPESHED_DIR} and ${TYPESHED_FQN} for substitutions.

(To limit mistakes, the substitutions use the contexts:
   "${ROOT_DIR}/
   "${ROOT_FQN}.
   "${TYPESHED_DIR}/
   "${TYPESHED_FQN}.
)
"""

import os
import sys

FROM_DIR = os.path.abspath(sys.argv[1])
TO_DIR = os.path.abspath(sys.argv[2])
TYPESHED_DIR = os.path.abspath(sys.argv[3])

ROOT_DIR = os.path.abspath(os.path.join(TO_DIR, '..'))
ROOT_DIR_PAT = '"${ROOT_DIR}' + '/'
ROOT_DIR_REPL = '"' + ROOT_DIR + '/'

ROOT_FQN = ROOT_DIR.replace('/', '.')
ROOT_FQN_PAT = '"${ROOT_FQN}' + '.'
ROOT_FQN_REPL = '"' + ROOT_FQN + '.'

TYPESHED_DIR = os.path.abspath(TYPESHED_DIR)
TYPESHED_DIR_PAT = '"${TYPESHED_DIR}' + '/'
TYPESHED_DIR_REPL = '"' + TYPESHED_DIR + '/'

TYPESHED_FQN = TYPESHED_DIR.replace('/', '.')
TYPESHED_FQN_PAT = '"${TYPESHED_FQN}' + '.'
TYPESHED_FQN_REPL = '"' + TYPESHED_FQN + '.'

assert not FROM_DIR.endswith('/')
assert not TO_DIR.endswith('/')

def cp_file(path_in, path_out):
    """Copy file, making subsitutions."""
    dir_out = os.path.dirname(path_out)
    try:
        os.makedirs(dir_out)
    except FileExistsError:
        pass
    with open(path_in, 'r') as file_in:
        contents = file_in.read()
    contents = contents.replace(ROOT_DIR_PAT, ROOT_DIR_REPL)
    contents = contents.replace(ROOT_FQN_PAT, ROOT_FQN_REPL)
    contents = contents.replace(TYPESHED_DIR_PAT, TYPESHED_DIR_REPL)
    contents = contents.replace(TYPESHED_FQN_PAT, TYPESHED_FQN_REPL)
    with open(path_out, 'w') as file_out:
        file_out.write(contents)

FROM_DIR_LEN = len(FROM_DIR) + 1  # +1 for trailing '/'
for root, _dir, files in os.walk(FROM_DIR):
    assert root.startswith(FROM_DIR)
    root_rest = root[FROM_DIR_LEN:]
    for file in files:
        cp_file(os.path.join(root, file), os.path.join(TO_DIR, root_rest, file))
