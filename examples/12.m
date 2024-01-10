# Infinite list

def ones():
  x = 0
  t = now()
  at t:
    x = (1, $x)
  x

def car(xs):
  xs[0]

def cdr(xs):
  xs[1]

u = ones()
print(car(u))
print(car(cdr(u)))
print(car(cdr(cdr(u))))

