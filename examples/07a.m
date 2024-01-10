# Time paradox
x = 1
t = now()
at t:
  x = $x
  print(x)
print(x)
