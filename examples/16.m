# Improved by mega

def FutureValue():
  x = 0
  t = now()
  def set(value):
    at t:
      x = $value
  (set, x)

def set(future, value):
  future[0](value)

def get(future):
  future[1]

##

f = FutureValue()
print(get(f))
set(f, 1)

