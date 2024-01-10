# (Prints 1 2 3)

t = now()
print(2)
at s:
  print(3)
at t:
  print(1)
  s = $(now())

