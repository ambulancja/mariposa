
def cons(elem, lst):
  (elem, lst)

def car(lst):
  lst[0]

def cdr(lst):
  lst[1]

def snoc(lst, elem):
  if lst == ():
    (elem, ())
  else:
    cons(car(lst), snoc(cdr(lst), elem))

def reverse(lst):
  if lst == ():
    ()
  else:
    snoc(reverse(cdr(lst)), car(lst))

def map(f, lst):
  if lst == ():
    None
  else:
    f(car(lst))
    map(f, cdr(lst))

def generate(n):
  if n == 0:
    ()
  else:
    cons(now(), generate(n - 1))

def pr(t):
  at t:
    print($t)

map(pr, reverse(generate(5)))

