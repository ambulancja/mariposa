
# FutureValue perfeccionado 

def FutureValue():
  x = None
  [now(), x]

def get(future):
  future[1]

def set(future, value):
  at future[0]:
    x = $value
    $(future[0]) = now() # nuevo

###

t = FutureValue()
print(get(t))
for i in range(10):
  set(t, i)

