
def FutureValue():
  x = 0
  (now(), x)

def set(future, value):
  at future[0]:
    x = $value

def get(future):
  future[1]

##

f = FutureValue()
print(get(f))
set(f, 1)

