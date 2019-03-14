# TODO: delete this file, after incorporating its test elsewhere

# Mostly this is tests of subscript assignment.


class A:
    #- { @aa defines/binding A_aa? }
    aa = 1.0
    #- { @bb defines/binding A_bb? }
    bb = []


# DictSetMakerNode
x = {}

# ListMakerNode
y = [1,2]

y[0] = A()

#- { @aa ref A_aa }
y[0].aa

# TODO: the following produces builtins.int.strip instead of builtins.str.strip
#       - better type inferencing is needed, but this is bad coding style anyway.
z = [[0],1]
z[0][0] = 'abc'
#- { @strip ref STRIP_? }
z[0][0].strip()

class D:
    def __init__(self):
        self.a = [A()]

d = D()
#- { @aa ref A_aa }
d.a[0].aa


#- { @bb ref A_bb }
d.a[0].bb[0] = 'abc'

#- { @strip ref STRIP }
d.a[0].bb[0].strip()

#- { @strip ref STRIP? }
'abc'.strip()
