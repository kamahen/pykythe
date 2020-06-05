# Test of os.path.sep (see https://github.com/kamahen/pykythe/issues/29)
# If you change this, also change os_path_sep1.py
# TODO: add verifier comments

import os  # This is different from os_path_sep1.py

print(os.path)
print(os.path.sep)
print(os.path.sep.strip())
print(os.path.sep.strip().encode())
