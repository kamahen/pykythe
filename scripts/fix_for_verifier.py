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

import os
import stat
import sys

VERSION = sys.argv[1].encode('ascii')
FROM_DIR, TO_DIR, TYPESHED_DIR, BUILTINS_DIR, FROM_FILE, TO_FILE = map(
        os.path.abspath, sys.argv[2:])

# In the following, ROOT_FQN_REPL is the same as ROOT_FQN.  This is
# because we used to check for a double-quote ('"') before the
# pattern, assuming that it was in something like
# 'vname("${ROOT_FQN}.foo.bar", ...)'. But this is not always the case
# (e.g., 'vname("<unknown>.{${ROOT_DIR}...", ...)', so this slight bit
# of extra safety has been removed.

VERSION_PAT = b'${VERSION}'
VERSION_REPL = VERSION

# In the following, the [1:] is to remove the leading "/".
# The "." is put into the FQN (for now).
# See pykythe:transform_kythe_path/2.
# TODO: align xxx_DIR, xxx_FQN so that there's no leading '.' in xxx_FQN
# TODO: Issue #24

# The .encode('ascii') stuff is for handling, e.g., iso-8859-1
# encoding ... we assume that the substitution patterns are ASCII and
# do all I/O in binary. This avoids having to figure out the encoding
# of the file.

ROOT_DIR = os.path.abspath(os.path.join(TO_DIR, '..'))[1:].encode('ascii')
ROOT_DIR_PAT = b'${ROOT_DIR}'  # followed by nothing or '/'
ROOT_DIR_REPL = ROOT_DIR

ROOT_FQN = ROOT_DIR.replace(b'/', b'.')
ROOT_FQN_PAT = b'${ROOT_FQN}'  # followed by nothing or '.'
ROOT_FQN_REPL = b'.' + ROOT_FQN

ROOT_UP_DIR = os.path.abspath(os.path.join(TO_DIR, '..', '..'))[1:].encode('ascii')
ROOT_UP_DIR_PAT = b'${ROOT_UP_DIR}'  # followed by nothing or '/'
ROOT_UP_DIR_REPL = ROOT_UP_DIR

ROOT_UP_FQN = ROOT_UP_DIR.replace(b'/', b'.')
ROOT_UP_FQN_PAT = b'${ROOT_UP_FQN}'  # followed by nothing or '.'
ROOT_UP_FQN_REPL = b'.' + ROOT_UP_FQN

TYPESHED_DIR = os.path.abspath(TYPESHED_DIR)[1:].encode('ascii')
TYPESHED_DIR_PAT = b'${TYPESHED_DIR}'  # followed by nothing or '/'
TYPESHED_DIR_REPL = TYPESHED_DIR

TYPESHED_FQN = TYPESHED_DIR.replace(b'/', b'.')
TYPESHED_FQN_PAT = b'${TYPESHED_FQN}'  # followed by nothing or '.'
TYPESHED_FQN_REPL = b'.' + TYPESHED_FQN

BUILTINS_DIR = os.path.abspath(BUILTINS_DIR)[1:].encode('ascii')
BUILTINS_DIR_PAT = b'${BUILTINS_DIR}'  # followed by nothing or '/'
BUILTINS_DIR_REPL = BUILTINS_DIR

BUILTINS_FQN = BUILTINS_DIR.replace(b'/', b'.')
BUILTINS_FQN_PAT = b'${BUILTINS_FQN}'  # followed by nothing or '.'
BUILTINS_FQN_REPL = b'.' + BUILTINS_FQN

assert FROM_DIR[-1] != b'/'
assert TO_DIR[-1] != b'/'


def cp_file(path_in, path_out):
    """Copy file, making subsitutions."""
    dir_out = os.path.dirname(path_out)
    try:
        os.makedirs(dir_out)
    except FileExistsError:
        pass
    with open(path_in, 'rb') as file_in:
        contents = file_in.read()
    contents = contents.replace(VERSION_PAT, VERSION_REPL)
    contents = contents.replace(ROOT_UP_DIR_PAT, ROOT_UP_DIR_REPL)
    contents = contents.replace(ROOT_UP_FQN_PAT, ROOT_UP_FQN_REPL)
    contents = contents.replace(ROOT_DIR_PAT, ROOT_DIR_REPL)
    contents = contents.replace(ROOT_FQN_PAT, ROOT_FQN_REPL)
    contents = contents.replace(TYPESHED_DIR_PAT, TYPESHED_DIR_REPL)
    contents = contents.replace(TYPESHED_FQN_PAT, TYPESHED_FQN_REPL)
    contents = contents.replace(BUILTINS_DIR_PAT, BUILTINS_DIR_REPL)
    contents = contents.replace(BUILTINS_FQN_PAT, BUILTINS_FQN_REPL)
    try:
        with open(path_out, 'rb') as file_orig:
            contents_orig = file_orig.read()
            contents_same = (contents_orig == contents)
    except FileNotFoundError:
        contents_same = False
    if not contents_same:
        try:
            os.remove(path_out)
        except FileNotFoundError:
            pass
        with open(path_out, 'wb') as file_out:
            file_out.write(contents)
            os.chmod(path_out,
                     os.stat(path_out).st_mode & ~(stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH))


if __name__ == '__main__':
    cp_file(FROM_FILE, TO_FILE)
