# DO NOT SUBMIT - delete this file


# Bug with "import..." (not "from...import...")
# - It seems that the symtab for an "import" isn't brought in properly.

class C:
    def a(self): pass
    b = 1


def foo():
    c = C()
    #- { @foo defines/binding CFoo? }
    c.foo = 1

# DO NOT SUBMIT
# Bug with unknown class ... the following works but will be
# an error if class C is defined elsewhere:
def bar(c):
    #- // { @foo defines/binding CFoo? }  // DO NOT SUBMIT - should get C.foo as a possibility
    c.foo = 1


import lib2to3.pytree  # For guessing attr 'prefix'  # DO NOT SUBMIT - reinstate this line
from lib2to3 import pytree  # DO NOT SUBMIT -- remove this - it works (with suitable changes below)
#   (DO NOT SUBMIT -- the "from lib2to3 import pytree" is necessary to get lib2to3.pytree
#   into symtab, but that still isn't sufficient, it seems to resolve lib2to3.pytree.Base.prefix
#   by guessing)

#- // { @lib2to3 ref LIB2TO3? }
#- // { @pytree ref PYTREE? }
#- // // { @Leaf ref LEAF? }  // DO NOT SUBMIT - reinstate
# foo = lib2to3.pytree.Leaf(1, "foo")  # DO NOT SUBMIT

def Assign2(source): # Note: no type annotation
    # The same as Assign but without the `source = [source]` line
    #- // { @prefix defines/binding SourcePrefix2? }  // DO NOT SUBMIT -- why doesn't this work?
    source.prefix = " "
    #- // TODO: @prefix_zzz should have a diagnostic "can't resolve"
    #- // { @prefix_zzz defines/binding SourcePrefixXXX? }  // TODO: Verify that there is *no* anchor here
    source.prefix_zzz = "xxx"
    return C(source)

def Assign3(source2):
    #- // TODO: @prefix_zzz should have a diagnostic "can't resolve"
    #- // { @prefix_zzz ref SourcePrefixXXX? }  //  // TODO: Verify that there is *no* anchor here
    print(source2.prefix_zzz)
    #- { @prefix ref SourcePrefix3? }  // DO NOT SUBMIT - reactivate
    return source2.prefix
