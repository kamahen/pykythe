try:
    # TODO: should this include "<unknown>"?
    # TODO: should there be one or two "/"s?
    #- { @foo4 defines/binding _ }
    #- { @foo4  ref/imports FOO4_imports1? } // vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo4", _, _, "", python) } // DO NOT SUBMIT
    #- { @zot   ref/imports ZOT_imports1? }  //  vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot",      _, _, "", python) } // DO NOT SUBMIT
    #- { @yyy   ref/imports YYY_imports1? }  //  vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy",          _, _, "", python) } // DO NOT SUBMIT
    #- { @#1"." ref/imports DOT1_imports1? } // vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1",              _, _, "", python) }  // DO NOT SUBMIT
    #- { @#0"." ref/imports DOT0_imports1? } // vname("<unknown>/${ROOT_DIR}/test_data", _, _, "", python) }  // DO NOT SUBMIT
    #- { @foo_bar5 defines/binding _ }
    #- { @foo5     ref/imports FOO5_imports1? } //  vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo5", _, _, "", python) } // DO NOT SUBMIT
    #- // { @foo_bar5 ref/imports FOO_BAR5_imports1? // vname("<unknown>/${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo5", _, _, "", python) } // DO NOT SUBMIT
    from . . yyy .zot import foo4, foo5 as foo_bar5
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'pykythe.test_data.imports_dir1.yyy'", [exc]

try:
    #- { @abc  ref/imports ABC_imports2? } // vname("<unknown>/qqsv/zot/abc", _, _, "", python) } // DO NOT SUBMIT
    #- { @zot  ref/imports ZOT_imports2? } // vname("<unknown>/qqsv/zot",     _, _, "", python) } // DO NOT SUBMIT
    #- { @qqsv ref/imports QQSV_imports2? } //  vname("<unknown>/qqsv",         _, _, "", python) } // DO NOT SUBMIT
    from qqsv.zot import abc
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'qqsv'", [exc]

try:
    #- { @abc ref/imports ABC_imports3? } // vname("<unknown>/xyz/zot/abc", _, _, "", python) } // DO NOT SUBMIT
    #- { @zot ref/imports ZOT_imports3? } // vname("<unknown>/xyz/zot",     _, _, "", python) } // DO NOT SUBMIT
    #- { @xyz ref/imports XYZ_imports3? } //  vname("<unknown>/xyz",         _, _, "", python) } // DO NOT SUBMIT
    import xyz.zot.abc
except ModuleNotFoundError as exc:
    assert exc.msg == "No module named 'xyz'", [exc]

try:
    #- @foo4 ref Foo4
    foo4

    #- @foo4 ref Foo4
    #- { @x ref Foo4X? } // =vname("<unknown>.{${ROOT_DIR}/test_data/imports_dir1/yyy/zot/foo4}.x", _, _, "", python) }
    foo4.x

    fff = foo4

    #- { @x ref Foo4X_2? }
    fff.x

except NameError:
    pass  # name 'foo4' is not defined

# TODO: add some simple tests of the imports (e.g. foo4.bar, foo_bar5.bar)
