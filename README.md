# Mariposa: programs that travel in time

Mariposa is a tiny toy programming language inspired by Python and intended to explore the possibility of programs having the ability to "travel in time".

## Travelling to the past

A basic example is the following program, which travels back in time:
```
x = 1
t = now()
print(x)
at t:
  x = 2
```
The variable `x` starts taking the value `1`. The value of the current instant is then saved in the local variable `t`, using the primitive function `now()`. The value of `x` is then printed. In a typical programming language, this would print the string `1` to standard output. However, in Mariposa, there is a possibility for the program to travel back in time and modify the value of `x`. The command `at t: <block>` executes the code block at the time indicated by `t`. The effect of this command is equivalent to modifying the value of `x` after the `t = now()` statement but before the `print(x)` statement. The result of running the program is then:
```
$ mariposa examples/01.m
2
```

It is not necessary for the instant returned by the `now()` operation to be used as the destination of time travel. However, it is not possible to travel more than once at the same time:
```
def main():
  t = now()
  at t:
    print(1)
  at t:
    print(2)

main() # ERROR: Multiple travelers to single point in time.
```

## Instants capture the current environment

The following is an example of a function `f` that modifies the value of the variable `x` at time `t` received as parameter:
```
def f(t):
  at t:
    x = 2

def main():
  x = 1
  t = now()
  print(x)
  f(t)

main() # Prints 2
```
The effect of this program is similar to the one above, that is, it prints `2` on the screen. Any reader who has a sensitivity about programming languages will have noticed that the assignment `x = 2` is statically outside the scope of the declaration of the variable `x`. As a very questionable design decision, in Mariposa the current instant returned by the `now()` primitive captures the current environment, and the command `at t: ...` "reopens" the dynamically captured environment.

A limitation of the current implementation of Mariposa, which could be improved in future versions of the language, is that variables assigned within an `at t:...` block must necessarily be variables that are declared in the captured environment. Declared variables are considered to be all those that are on the left side of an assignment. For example, the following program produces an error, indicating that the variable `x` is not declared:
```
def f(t):
  at t:
    x = 2

def main():
  t = now()
  print(x)
  f(t)

main() # ERROR: Unbound variable: x
```

## Reading from the parent time frame

In an example like the one above, we might want to make an assignment `x = y`, where `y` is some value computed inside the function `f` but outside of the `at` command. One could attempt the following code, with the aim of giving `x` the value `2`:
```
def f(t):
  y = 2
  at t:
    x = y

def main():
  x = 1
  t = now()
  print(x)
  f(t)

main() # ERROR: Unbound variable: y
```
However, this produces an error, indicating that the variable `y` is not declared, as the body of the `at t: ...` block is executed in the environment of the `main` function, which does not include a declaration for `and`. To solve this, one can write `$y`, which refers to the value of `y` before starting the execution of the `at` block (i.e. "before stepping into the time machine"). In general, Mariposa has a stack of time frames. Every time the execution of an `at` block starts, it is entered into a nested time frame. If `expr` is an arbitrary expression, `$(expr)` denotes the value of the expression in the parent time frame. Thus, we have:
```
def f(t):
  y = 2
  at t:
    x = $y

def main():
  x = 1
  t = now()
  print(x)
  f(t)

main() # Prints 2
```

The `$` operator can be used in a nested manner:
```
def g(t2):
  y = 2
  at t2:
    at t1:
      x = $$y

def f(t1):
  t2 = now()
  g(t2)

def main():
  x = 1
  t1 = now()
  print(x)
  f(t1)

main() # Prints 2
```

However, it is not possible to exit outside the original time frame, which is the
which starts the execution of the program:
```
def main():
  print($1)

main() # ERROR: Cannot refer to time outside the origin.
```

## Writing to parent time frames

Variables from the parent timeframe can also be written, using the
syntax `$x = expr`:
```
def f(t):
  y = 3
  at t:
    $y = x
    print(y)

def main():
  x = 1
  t = now()
  x = 2
  f(t)

main() # Prints 1
```

It is interesting to note that this does not require adding functionalities fundamentally new to the language as, in fact, `$x = expr` is converted, by means of a _desugaring_ procedure, to:
```
at $(now()):
  x = $(expr)
```

## Travelling to the future

The first example illustrates that programs can travel back in time, using the command `at t: ...`, with a variable `t` that references an instant in the past. Could we also travel to the future? At first glance this does not seem possible, since we should have a variable `t_future` that refers to an instant in the future. But this is possible with the following technique, which "travels to the present" from the future to set the value of `t_future`:
```
def main():
  t_past = now()
  print("We are in the present")
  at t_future:
    print("We are in the future")
  print("...Many years pass...")
  at t_past:
    t_future = $(now())

main()
  # Prints:
  #   We are in the present
  #   ...Many years pass...
  #   We are in the future
```

## Ideas behind the implementation

The Mariposa interpreter implementation is based on the following ideas, with various exceptions and imperfections:
* The interpreter keeps a "timeline" that establishes a total order over the set of all known instants. Every time the `now()` primitive is called, a new instant is generated. Other instructions can also generate new instants to maintain the sequential order of execution and enable time travel. Instants are identified by a number that only serves as an identifier, and is not necessarily related to the relative chronological order.
* Local variables are bound to memory addresses. Memory is indexed by a memory location and an instant. Each memory cell contains a value. Values can be _proper_ values (booleans, integers, closures, tuples, etc.) or _improper_ values. An improper value is a pair `(addr, i)`, representing the contents of the memory cell at address `addr` at time `i`. The thing to keep in mind here is that the value of the cell could be determined in the future of the execution trace.
* The interpreter manipulates proper or improper values without distinction, until the time in which an input/output operation or a control flow operation depends on observing an improper value. At that point, _resolution_ of the improper value is performed to convert it into a proper value. Resolving an improper value `(addr, i)` consists of looking at the value of the memory cell at address `addr` at all times chronologically prior to `i`, taking the most recent value.
* Once the improper value `(addr, i)` has been resolved, its value is given by the value at address `addr` at a certain point in time `j`, with `j` before or equal to `i`. This _disables_ the ability to mutate all memory cells at address `addr` with times between `j` and `i`.
* Input/output operations (`input`/`print`) are registered in an event queue that is flushed only when the program ends, in chronological order depending on the instant in which they occurred. This allows input/output operations to be issued out of order throughout the execution. An exception to this rule is the following: when an improper value that depends on a read operation is resolved, all pending I/O operations up to that point are performed, and read operations backward are _disabled_.

## Temporal paradoxes

The assignment of a variable corresponds to the mutation of the value contained in a cell. Time travel can partially affect the flow of control:
```
def main():
flag = True
t = now()
if flag:
  print("elephant")
else:
  print("butterfly")
at t:
  flag = False

main() # Prints "butterfly"
```
However, if the control flow depends on a value and one tries to modify it, this leads to a temporal paradox:
```
def main():
  flag = True
  t = now()
  if flag:
    at t:
      flag = False

main() # ERROR: Assignment would lead to time paradox.
```

A temporal paradox also occurs if one tries to modify a value that depends on itself:
```
def main():
  x = 1
  t = now()
  at t:
    x = $x + 1
  print(x)

main() # ERROR: Time paradox: value depends on itself.
```

However, it is legitimate to produce a cyclic data structure:
```
def main():
  x = 1
  t = now()
  at t:
    x = [$x, $x]
  print(len(x))

main() # Prints 2
       # (The final value of x is an infinite binary tree)
```

## Time investment

An interesting feature of Mariposa is that it allows to manipulate instants within data structures. The following example creates a list of instants `[now(), now(), now(), now(), now()]`. The instants are ordered chronologically because evaluation in Mariposa proceeds from left to right. The list is inverted, and an effect is produced at each instant contained in the list.
```
def reverse(list):
  res = []
  for x in list:
    res = [x] + res
  res

i = 0
for t in reverse([now(), now(), now(), now(), now()]):
  at t:
    print($i)
  i = i + 1
```
This produces as a result:
```
4
3
2
1
0
```

## The future value technique

The following program prints `1`:
```
def main():
  x = 0
  z = (now(), x)
  print(z[1])
  at z[0]:
    x = 1

main() # Prints 1
```
In this snippet, `z` is a tuple containing an instant and a value (technically, an _improper_ value), which corresponds to the value found in the memory address to which the variable `x` is bound. It is important to note that the program depends on the fact that the evaluation in Mariposa proceeds from left to right.

This idea can be generalized to a technique that allows creating a "future value" `f`, with a `get(f)` operation that returns the value that will be given to `f` at some point in the future, and a `set(f, x)` operation that gives the value `x` to `f`:

```
def FutureValue():
  x = 0
  (now(), x)

def set(future, value):
  at future[0]:
    x = $value

def get(future):
  future[1]

def main():
  f = FutureValue()
  print(get(f))
  set(f, 1)

main() # Prints 1
```
In the above program, a future value `f` is created, its future value is displayed and only then is it given the value `1`. This technique allows only to set a value for `f` at most once. If one calls `set(f, ...)` more than once, one gets the error `Multiple travelers to single point in time`.

## Time chaining

It is not possible to travel more than once at the same instant in time. However, it is possible to travel through time to the `t` that perform certain actions and _also_ modify the value of `t` in the parent time frame, like when chaining stitches, so that it refers to a new instant that could be the destiny of future travels in time.

For example, the following program fails because it attempts to travel more than one time at the same instant:

```
def main():
  t = now()
  for i in range(10):
    at t:
      print($i)

main() # ERROR: Multiple travelers to single point in time.
```

On the other hand, the following program works, using the chaining technique:

```
def main():
  t = now()
  for i in range(10):
    at t:
      print($i)
      $t = now()

main() # Prints 0 1 ... 9
```

A curious remark is that the same chaining can be done _backwards_ (towards the past):

```
def main():
  t = now()
  for i in range(10):
    at t:
      $t = now()
      print($i)

main() # Prints 9 ... 1 0
```

Using the chaining technique, an improved version of the future values technique, which allows `set(f, ...)` to be invoked for an indefinite number times:

```
def FutureValue():
  x = None
  [now(), x]

def get(future):
  future[1]

def set(future, value):
  at future[0]:
    x = $value
    $(future[0]) = now()

def main():
  f = FutureValue()
  print(get(f))
  for i in range(10):
    set(f, i)

main() # Prints 9
```

## Features implemented (with bugs)

* Time travel primitives: `now()`, `at..` and `$(...)`.
* `def` with fixed number of parameters.
* `print` and `input`.
* Booleans (`True`, `False`) with logical operations (`and`, `or`, `not`).
* Integers with arithmetic operations (`+`, `-`, `*`, `/`, `%`, `**`), with integer division.
* Relational operators (`==`, `!=`, `<`, `<=`, `>`, `>=`).
* Single and multi-line strings with escapes of special and hexadecimal characters.
* Strings, immutable tuples, and mutable lists implementing `len` and indexing, with possibly negative indexes.
* _destructuring_ assignment whose LHS can include tuples and lists (eg `x, y = y, x`).
* `if..elif..elif..else..`
* `while`
* `for..in..` on strings, tuples and lists. Translated to a `while` by the desugaring process.
* `range(n)` with a single parameter. Returns a list (as in old Python 2).

## Pending or unimplemented features

* `return`, `break`, `continue` seem difficult, the interpreter would probably have to be rewritten in CPS.
* `at` at the expression level, `x@t`.
* `__getitem__` and `__setitem__` currently force resolution of the values they take as arguments, they could be made _lazy_.
* `range` with more than one argument.
* The `if` and `while` do not currently behave consistently. The `if` is deferred without forcing the resolution of the condition, while the `while` always forces the resolution of the condition. This is arbitrary, and could be done in other ways.
* Dictionaries and sets.
* Classes, objects,
* Notations `x += 1`, `x -= 1`, etc.
* Slicing of lists (_slices_).
* List comprehensions.
* Generators.
* Files.
* Exceptions.
* Floating point numbers.
* _bitwise_ operations.
* Decimal and octal escapes in strings.
* Modules.
* (Etc).

## Semantics and correctness of implementation

It is not obvious how the Mariposa language could be given a formal semantics. Since there is no specification of its semantics, it doesn't even make sense to ask if the implementation is right or wrong. However, we are sure that the implementation is incorrect for almost any conceivable semantics. It should be understood only as an exploratory game.

The implementation is currently inefficient and relies on a suboptimal data structure to operate with timelines. This could be improved.

No bugs will be fixed in the future (but perhaps in the past).

