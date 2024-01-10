
def f(t1, t2, t3):
  at t1:
    x = 2 * x
  at t2:
    x = x + 1
  at t3:
    x = x * x

x = 6
t1 = now()
t2 = now()
t3 = now()
f(t3, t2, t1)
print(x)

