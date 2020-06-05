# Test resolution of inherited methods
# TODO: delete


class C0:

    def __init__(self, x) -> None:
        self.x = x

    def inherited(self) -> int:
        return self.x


class C1(C0):
    pass


class C2(C0):

    def inherited(self) -> int:
        return 2 * self.x


def foo() -> None:
    c0 = C0(1)
    c1 = C1(2)
    c2 = C2(3)

    print(c0.inherited())
    print(c1.inherited())
    print(c2.inherited())

def bar() -> None:
    cs = [C0(10), C1(2), C2(3)]
    print(cs[0])
    for c in cs:
        print(c)
