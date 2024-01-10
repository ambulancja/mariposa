def f(t):
  y = 3
  at t:
    x = $y   # $y refers to the value of y
             # at the point when doing the 'at'

x = 1
y = 2
t = now()
print(x)
f(t)
