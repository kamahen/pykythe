
try:
    # TODO: should this include "<unknown>"?
    # TODO: should there be one or two "/"s?
    #- { @foo4 defines/binding _ }
    #- { @foo4  ref/file vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo4", _, _, "", python) }
    #- { @zot   ref/file vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot",      _, _, "", python) }
    #- { @yyy   ref/file vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy",          _, _, "", python) }
    #- { @#1"." ref/file vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1",              _, _, "", python) }
    #- { @#0"." ref/file vname("<unknown>/${ROOT_DIR}/test_data", _, _, "", python) }
    #- { @foo_bar5 defines/binding _ }
    #- { @foo5     ref/file vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo5", _, _, "", python) }
    #- // { @foo_bar5 ref/file vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo5", _, _, "", python) }  // TODO: why is this missing the "<unknown>"?
    from . . yyy .zot import foo4, foo5 as foo_bar5
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'pykythe.test_data.imports_dir1.yyy'", [exc]

try:
    #- { @abc  ref/file vname("<unknown>/qqsv/zot/abc", _, _, "", python) }
    #- { @zot  ref/file vname("<unknown>/qqsv/zot",     _, _, "", python) }
    #- { @qqsv ref/file vname("<unknown>/qqsv",         _, _, "", python) }
    from qqsv.zot import abc
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'qqsv'", [exc]

try:
    #- { @abc ref/file vname("<unknown>/xyz/zot/abc", _, _, "", python) }
    #- { @zot ref/file vname("<unknown>/xyz/zot",     _, _, "", python) }
    #- { @xyz ref/file vname("<unknown>/xyz",         _, _, "", python) }
    import xyz.zot.abc
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'xyz'", [exc]

try:
    #- @foo4 ref Foo4
    foo4

    #- @foo4 ref Foo4
    #- { @x ref Foo4X=vname("<unknown>.{${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo4}.x", _, _, "", python) }
    foo4.x

    fff = foo4

    #- @x ref Foo4X
    fff.x

except NameError:
    pass  # name 'foo4' is not defined

# TODO: add some simple tests of the imports (e.g. foo4.bar, foo_bar5.bar)
