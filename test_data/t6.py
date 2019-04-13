# TODO: delete -- add this test to the repetoire

class C:
    def __init__(self, source):
        self.f1 = source

def Assign(source):
    source.prefix = " "
    source = [source]
    return C(source)

# Just for fun:
def t():
    print(Assign(C('abc')).f1[0].f1)
