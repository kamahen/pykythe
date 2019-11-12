# TODO: delete -- add this test to the repetoire

import lib2to3.pytree  # For guessing attr 'prefix'

class C:
    def __init__(self, source: C):
        #- { @f1 defines/binding F1_def? }
        self.f1 = source

    def some_call(self):
        return 123

def Assign(source): # Note: no type annotation
    #- // @prefix defines/binding SourcePrefix?  // Without { ... } to allow backtracking  DO NOT SUBMIT -- why doesn't this work?  (see below)
    source.prefix = " "
    # TODO: the following causes the source.prefix anchor to not be output
    #       because we don't handle the situation of source being iniitally
    #       unknown and then getting list_of_type.
    #       This can be solved by doing SSA analysis.
    source = [source]
    return C(source)

def Assign2(source): # Note: no type annotation
    # The same as Assign but without the `source = [source]` line
    #- // @prefix defines/binding SourcePrefix2?  // DO NOT SUBMIT -- why doesn't this work?
    source.prefix = " "
    return C(source)

#- { @foo defines/binding Assign_foo? }
Assign.foo = 'bar'

#- { @foo ref Assign_foo? }
print(Assign.foo)

def t(xyz):
    #- { @#0f1 ref F1_use? } // TODO: should get C.f1
    #- { @#1f1 ref F1_use2? }
    print(Assign(C('abc')).f1[0].f1)

    #- { @some_call ref C_some_call1? }
    print(C('abc').some_call())

    #- { @some_call ref C_some_call2? }
    print(xyz.some_call())

    #- { @f1 ref F1_use3? }
    print(xyz.f1)
