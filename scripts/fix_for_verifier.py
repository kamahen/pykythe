#!/usr/bin/env python3.7
"""Read files and do substitutions that make it suitable for Kythe verifier.

Takes three arguments: from_dir to_dir typeshed_dir from_file

Does a copy from from_dir to to_dir, with substitutions. The output
files are made read-only. Output is only done if a file has changed or
it doesn't exist -- this is so that Make doesn't get confused by new
file modification timestamps.

The 'root path' is the absolute form of one directory up from to_dir
(that is, root_dir is the absolute form of 'to_dir/..'). This
replaces ${ROOT_DIR} in the file. ${ROOT_FQN} gets the same
subsitution, except with '.'s instead of '/s'.

The 'typeshed path' is the absolute form of typeshed_dir.
It uses ${TYPESHED_DIR} and ${TYPESHED_FQN} for substitutions.
"""

import os, stat, sys

VERSION = sys.argv[1]
FROM_DIR, TO_DIR, TYPESHED_DIR, FROM_FILE, TO_FILE = map(os.path.abspath, sys.argv[2:])

# In the following, ROOT_FQN_REPL is the same as ROOT_FQN.  This is
# because we used to check for a double-quote ('"') before the
# pattern, assuming that it was in something like
# 'vname("${ROOT_FQN}.foo.bar", ...)'. But this is not always the case
# (e.g., 'vname("<unknown>.{${ROOT_DIR}...", ...)', so this slight bit
# of extra safety has been removed.

VERSION_PAT = '${VERSION}'
VERSION_REPL = VERSION

ROOT_DIR = os.path.abspath(os.path.join(TO_DIR, '..'))
ROOT_DIR_PAT = '${ROOT_DIR}'  # followed by nothing or '/'
ROOT_DIR_REPL = ROOT_DIR

ROOT_FQN = ROOT_DIR.replace('/', '.')
ROOT_FQN_PAT = '${ROOT_FQN}'  # followed by nothing or '.'
ROOT_FQN_REPL = ROOT_FQN

TYPESHED_DIR = os.path.abspath(TYPESHED_DIR)
TYPESHED_DIR_PAT = '${TYPESHED_DIR}'  # followed by nothing or '/'
TYPESHED_DIR_REPL = TYPESHED_DIR

TYPESHED_FQN = TYPESHED_DIR.replace('/', '.')
TYPESHED_FQN_PAT = '${TYPESHED_FQN}'  # followed by nothing or '.'
TYPESHED_FQN_REPL = TYPESHED_FQN

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
    contents = contents.replace(VERSION_PAT, VERSION_REPL)
    contents = contents.replace(ROOT_DIR_PAT, ROOT_DIR_REPL)
    contents = contents.replace(ROOT_FQN_PAT, ROOT_FQN_REPL)
    contents = contents.replace(TYPESHED_DIR_PAT, TYPESHED_DIR_REPL)
    contents = contents.replace(TYPESHED_FQN_PAT, TYPESHED_FQN_REPL)
    try:
        with open(path_out, 'r') as file_orig:
            contents_orig = file_orig.read()
            contents_same = (contents_orig == contents)
    except FileNotFoundError:
        contents_same = False
    if not contents_same:
        try:
            os.remove(path_out)
        except FileNotFoundError:
            pass
        with open(path_out, 'w') as file_out:
            file_out.write(contents)
            os.chmod(path_out,
                     os.stat(path_out).st_mode & ~(stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH))


if __name__ == '__main__':
    cp_file(FROM_FILE, TO_FILE)
