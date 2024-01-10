
# 1 2
def test1():
    i = 0
    t = now()
    flag = True
    if flag:
      print(1)
    if flag:
      print(2)

# (nothing)
def test2():
    i = 0
    flag = True
    t = now()
    if flag:
      print(1)
    if flag:
      print(2)
    at t:
      flag = False

# 1
def test3():
    i = 0
    flag = True
    t = now()
    if flag:
      print(1)
    t = now()
    if flag:
      print(2)
    at t:
      flag = False

print(999)
test1()
print(999)
test2()
print(999)
test3()
print(999)
