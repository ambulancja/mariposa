
def reverse(list):
  i = 0
  n = len(list)
  while i < n / 2:
    tmp = list[i]
    list[i] = list[n - 1 - i]
    list[n - i - 1] = tmp
    i = i + 1

N = 10
i = 0
l = []
while i < N:
  l = l + [now()]
  i = i + 1
reverse(l)
i = 0
while i < N:
  at l[i]:
    print($i)
  i = i + 1

