# TODO: mark this string as childof the package
""" Tests for the childof relation."""

from typing import Optional

one_global: int
two_global = 'xyz'
three_global: Optional[str] = None

class C1:
    f1: int
    f2 = 'xyz'
    f3: Optional[str] = None

    def __init__(self, f4):
        self.f4 = f4
        self.f1 = '---'

    def get_f2(self):
        return self.f2

    def get_f4(self):
        def inside():
            return self.f4
        return inside()

    def values(self):
        return [self.f1, self.f2, self.f3, self.f4]

    class C2:
        g1: int
        g2 = 'abc'
        g3: Optional[str] = None

        def __init__(self, g4):
            self.g4 = g4
            self.g1 = '***';

        def values(self):
            try:
                # self.f1 is an error
                return [self.g1, self.g2, self.g3, self.g4, self.f1]
            except Exception as e:
                return [self.g1, self.g2, self.g3, self.g4, e]

        def not_a_method(x):
            return x + C1.f2


c1 = C1(111)
print(c1.get_f4())
print(c1.values())

c2 = c1.C2(666)
print(c2.values())

print(C1.C2.not_a_method('--'))
