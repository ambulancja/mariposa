
l = [1]
print(l) # [1]
t = now()
print(l) # [2]
l = [3]
print(l) # [3]
at t:
  l = [2]
  $(l[0]) = 4
print(l) # [4]

