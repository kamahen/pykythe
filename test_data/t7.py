# TODO: delete (and incorporate the test elsewhere)

# This used to generate two facts; nonredundant_pytype_fact/2 fixes this.

#  App(fact, (App(vname, (.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.t7.B._init2, CORPUS, ROOT, "", python)), "", "", /pykythe/type, []))
#  App(fact, (App(vname, (.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.t7.B._init2, CORPUS, ROOT, "", python)), "", "", /pykythe/type, [function_type('.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.t7.B._init2',[[]],[])]))


class B:
    def __init__(self):
        #- { @"_init2" ref INIT2ref? }
        #- { INIT2ref./pykythe/type INIT2type? }
        self._init2()

    #- { @"_init2" defines/binding INIT2def? }
    #- { INIT2def./pykythe/type INIT2type_? }
    def _init2(self):
        pass
