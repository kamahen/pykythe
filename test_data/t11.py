# This code is from a pytype bug report
#     https://github.com/google/pytype/issues/450
# (I've moved one global binding to the end to make
# it more difficult)
# - pytype has problems with the "i":
# ERROR:pytype.analyze:No visible options for i
# line 12, in test: Name 'x' is not defined [name-error]
# line 12, in test: Name 'i' is not defined [name-error]

#- { @i defines/binding I }
i = 0

def test():
    #- { @x ref X }
    #- { @#1i ref I }
    print(x, i)

#- { @#1i ref I }
while i < 3:
    #- { @i ref I }
    i += 1

#- { @x defines/binding X }
x = 1


