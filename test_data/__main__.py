# From the test_data dir:
#   python3.7 -m test_data foo.py
#   python3.7 -m foo
# (assuming foo.py and __main__.py are the same)

from .dummy_dir import dummy_file
print(__package__)
print(__name__)
print(__file__)
