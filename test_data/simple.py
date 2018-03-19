"""Exercise some simple class/attribute bindings.

This code is for development debugging and will change a lot over time
(and eventually disappear).
"""

class C1:
    def __init__(self, x: int) -> None:
        self.x = x + 1

class C2:
    def __init__(self) -> None:
        self.x = x

c = C1()
print(c.x)

c = C2()
c.x

# TODO: The following have syntax errors (lib2to3 bug)
#       https://bugs.python.org/issue33064
# def foo(bar, *baz,): pass
# def foo(bar, **baz,): pass
# def foo(bar, *, baz,): pass
