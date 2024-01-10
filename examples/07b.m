# This would produce an infinite (cyclic) value
# Commented to avoid running it by mistake
#x = 1
#t = now()
#at t:
#  x = ($x, $x)
#  print(x)
#print(x)

#### Time paradox
x = 1
t = now()
at t:
  x = __add__($x, $x)
  print(x)
print(x)
