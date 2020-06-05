try:
    # TODO: Fix extra "."s with "<unknown>"s (see module_path.pl)
    #- { @foo4 defines/binding _ }
    #- { @foo4  ref/imports vname("<unknown>.${ROOT_UP_FQN}.pykythe.yyy.zot.foo4", _, _, "", python) }
    #- { @zot   ref/imports vname("<unknown>.${ROOT_UP_FQN}.pykythe.yyy.zot", _, _, "", python) }
    #- { @yyy   ref/imports vname("<unknown>.${ROOT_UP_FQN}.pykythe.yyy", _, _, "", python) }
    #- { @#1"." ref/imports vname("${ROOT_FQN}.test_data",  _, _, "", python) }
    #- { @#0"." ref/imports vname("${ROOT_UP_FQN}.pykythe", _, _, "", python) }
    #- { @foo_bar5 defines/binding _ }
    #- { @foo5  ref/imports vname("<unknown>.${ROOT_UP_FQN}.pykythe.yyy.zot.foo5", _, _, "", python) }
    #- { @foo_bar5 ref/imports vname("<unknown>.{/${ROOT_UP_DIR}/pykythe/yyy/zot/foo5}", _, _, "", python) }
    from . . yyy .zot import foo4, foo5 as foo_bar5
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'pykythe.test_data.imports_dir1.yyy'", [exc]

try:
    #- { @abc  ref/imports vname("<unknown>.$PYTHONPATH.qqsv.zot.abc", _, _, "", python) }
    #- { @zot  ref/imports vname("<unknown>.$PYTHONPATH.qqsv.zot",     _, _, "", python) }
    #- { @qqsv ref/imports vname("<unknown>.$PYTHONPATH.qqsv",         _, _, "", python) }
    from qqsv.zot import abc
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'qqsv'", [exc]

try:
    #- { @abc ref/imports vname("<unknown>.$PYTHONPATH.xyz.zot.abc", _, _, "", python) }
    #- { @zot ref/imports vname("<unknown>.$PYTHONPATH.xyz.zot",     _, _, "", python) }
    #- { @xyz ref/imports vname("<unknown>.$PYTHONPATH.xyz",         _, _, "", python) }
    import xyz.zot.abc
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'xyz'", [exc]

try:
    #- @foo4 ref Foo4
    foo4

    #- @foo4 ref Foo4
    #- { @x ref vname("<unknown>.{/${ROOT_UP_DIR}/pykythe/yyy/zot/foo4}.x", _, _, "", python) }
    foo4.x

    fff = foo4

    #- { @x ref vname("<unknown>.{/${ROOT_UP_DIR}/pykythe/yyy/zot/foo4}.x", _, _, "", python) }
    fff.x

except NameError:
    pass  # name 'foo4' is not defined
