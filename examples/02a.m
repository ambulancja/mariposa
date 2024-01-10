def f(t):
  y = 3
  at t:
    x = y  # y refers to the value of y at time t

x = 1
y = 2
t = now()
print(x)
f(t)
