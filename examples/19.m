
nil = ()

def cons(elem, lst):
  (elem, lst)

def car(lst):
  lst[0]

def cdr(lst):
  lst[1]

def snoc(lst, elem):
  if lst == nil:
    cons(elem, nil)
  else:
    cons(car(lst), snoc(cdr(lst), elem))

def reverse(lst):
  if lst == nil:
    nil
  else:
    snoc(reverse(cdr(lst)), car(lst))

def map(f, lst):
  if lst == nil:
    None
  else:
    f(car(lst))
    map(f, cdr(lst))

def list_at(lst, i):
  if i == 0:
    car(lst)
  else:
    list_at(cdr(lst), i - 1)

res = cons(now(), cons(now(), cons(now(), cons(now(), cons(now(), ())))))

def do(i):
  at list_at(res, i):
    print($i)

map(do, cons(1, cons(3, cons(2, cons(4, cons(0, ()))))))

