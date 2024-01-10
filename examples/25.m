
def concat_sep(separator, xs):
  res = ''
  i = 0
  separate = True
  t = now()
  while i < len(xs):
    res = res + xs[i]
    t = now()
    if separate:
      res = res + separator
    i = i + 1
  at t:
    separate = False
  res 

list = ["De", "estas", "calles", "que", "ahondan", "el", "poniente"]

print(concat_sep(", ", list))

