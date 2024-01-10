# Mariposa: programas que viajan en el tiempo

Mariposa es un lenguaje de programación de juguete minimalista inspirado en Python y destinado a explorar la posibilidad de los programas tengan la capacidad de "viajar en el tiempo".

## Viajes al pasado

Un ejemplo básico es el siguiente programa, que viaja al pasado:
```
x = 1
t = now()
print(x)
at t:
  x = 2
```
La variable `x` comienza tomando el valor `1`.
A continuación se guarda en la variable local `t` el valor del instante actual, usando la función primitiva `now()`.
Luego se imprime el valor de `x`. En un lenguaje de programación usual, esto haría que se imprima la cadena `1` en la salida estándar.
Sin embargo, en Mariposa, existe la posibilidad de que el programa viaje atrás en el tiempo y modifique el valor de `x`. El comando `at t: <bloque>` ejecuta el bloque de código en el instante indicado por `t`. El efecto de este comando es equivalente a modificar el valor de `x` después de la instrucción `t = now()` pero antes de la instrucción `print(x)`.
El resultado de ejecutar el programa es, entonces:
```
$ mariposa examples/01.m
2
```

No es necesario que el instante devuelto por la operación `now()` se utilice como destino
de un viaje en el tiempo.
Sin embargo, no es posible viajar más de una vez al mismo instante:
```
def main():
  t = now()
  at t:
    print(1)
  at t:
    print(2)

main() # ERROR: Multiple travelers to single point in time.
```

## El instante captura el entorno actual

El siguiente es un ejemplo de una función `f` que modifica el valor de la variable `x`
en el instante `t` recibido como parámetro:
```
def f(t):
  at t:
    x = 2

def main():
  x = 1
  t = now()
  print(x)
  f(t)

main() # Imprime 2
```
El efecto de este programa es similar al de más arriba, es decir, imprime `2` en pantalla.
Cualquierx lectorx que haya desarrollado cierta sensibilidad sobre lenguajes de programación
observará que la asignación `x = 2` está estáticamente por fuera del alcance de la declaración
de la variable `x`.
Como decisión, muy cuestionable, de diseño, en Mariposa el instante actual que
devuelve la primitiva `now()` captura el entorno actual,
y el comando `at t: ...` "reabre" el entorno capturado de manera dinámica.

Una limitación de la implementación actual de Mariposa, que podría subsanarse en futuras
versiones del lenguaje, es que las variables asignadas dentro de un bloque `at t: ...`
deben ser necesariamente variables que se encuentren declaradas en el entorno capturado
en el instante `t`. (Se consideran variables declaradas a todas aquellas que se encuentren
en el lado izquierdo de una asignación).
Por ejemplo, el siguiente programa produce un error, indicando que la variable `x` no
se encuentra declarada:
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

## Lectura de valores del marco temporal padre

En un ejemplo como el anterior, podríamos querer hacer una asignación `x = y`
donde `y` es algún valor computado dentro de la función `f` pero fuera del `at`.
Podríamos tratar de escribir esto, con la intención de darle a `x` el valor `2`:
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
Sin embargo, este programa produce un error, indicando que la variable `y` no
se encuentra declarada, ya que el cuerpo del bloque `at t: ...` se ejecuta en el
entorno de la función `main`, que no incluye una declaración para `y`.
Para solucionar esto se puede escribir `$y`, que se refiere al valor que tiene `y`
antes de comenzar la ejecución del bloque `at` (es decir, "antes de iniciar el viaje
en el tiempo").
En general, Mariposa cuenta con una pila de marcos temporales.
Cada vez que se inicia la ejecución de un bloque `at` se ingresa en
un marco temporal anidado.
Si `expr` es una expresión arbitraria, `$(expr)` denota el valor de la expresión
en el marco temporal padre. Así, se tiene que:
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

main() # Imprime 2
```

Se puede utilizar el operador `$` de manera anidada:
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

main() # Imprime 2
```

Sin embargo, no es posible salir fuera del marco temporal de origen, que es el
que da inicio a la ejecución del programa:
```
def main():
  print($1)

main() # ERROR: Cannot refer to time outside the origin.
```

## Escritura a valores del marco temporal padre

Las variables del marco temporal padre también se pueden escribir, usando la
sintaxis `$x = expr`:
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

main() # Imprime 1
```

Es interesante observar que esto no requiere agregar funcionalidades fundamentalmente
nuevas al lenguaje, ya que en efecto
`$x = expr` se convierte, por medio de un procedimiento de _desugaring_,
en:
```
at $(now()):
  x = $(expr)
```

## Viajes al futuro

El primer ejemplo ilustra que los programas pueden viajar al pasado, usando
el comando `at t: ...`, con una variable `t` que referencia un instante del pasado.
¿Podríamos viajar también al futuro? A primera vista esto no parece posible, ya que deberíamos
contar con una variable `t_futuro` que referencie un instante del futuro.
Pero esto es posible con la siguiente técnica, que "viaja al presente" desde el futuro
para establecer el valor de `t_futuro`:
```
t_pasado = now()
print("Estamos en el presente")
at t_futuro:
  print("Código que se ejecutará en el futuro")
print("...Pasan muchos años...")
at t_pasado:
  t_futuro = $(now())
```

## Ideas detrás de la implementación

La implementación del intérprete de Mariposa se basa en las siguientes ideas, con diversas excepciones e imperfecciones:
* El intérprete conoce una "línea de tiempo" que establece un orden total sobre el conjunto de todos los instantes conocidos. Cada vez que se invoca a la primitiva `now()` se genera un nuevo instante. Además, otras instrucciones pueden generar nuevos instantes para mantener el orden secuencial de ejecución y posibilitar viajes en el tiempo. Los instantes están identificados por un número que sólo hace las veces de identificador, pero no está necesariamente relacionado con el orden cronológico relativo.
* Las variables locales están ligadas a direcciones de memoria. La memoria está indexada por posiciones de memoria e instantes. Cada celda de memoria contiene un valor. Los valores pueden ser valores _propios_ (booleanos, enteros, clausuras, tuplas, etc.) o valores _impropios_. Un valor impropio es un par `(addr, i)` que representa el contenido de la celda de memoria en la dirección `addr` en el instante `i`. Lo importante es que el valor de celda podría ser determinado en el futuro de la traza de ejecución.
* El intérprete manipula indistintamente valores propios o impropios, hasta el momento en el que una operación de entrada/salida o una operación de flujo de control depende de observar un valor impropio. En ese momento, se efectúa la _resolución_ del valor impropio para convertirlo en un valor propio. La resolución de un valor impropio `(addr, i)` consiste en mirar el valor de la celda de memoria en la dirección `addr` en todos los instantes cronológicamente anteriores a `i`, tomando el valor más reciente.
* Una vez que el valor impropio `(addr, i)` fue resuelto, su valor estará dado por el que había en la dirección `addr` en un instante `j` con `j` anterior o igual a `i`. Esto _inhabilita_ la posibilidad de mutar todas las celdas de memoria en la dirección `addr` con instantes comprendidos entre `j` e `i`.
* Las operaciones de entrada/salida (`input`/`print`) se registran en una cola de eventos que se _flushea_ recién cuando finaliza el programa, por orden cronológico dependiendo en el instante en el que ocurrieron. Esto permite que las operaciones de entrada/salida se emitan en desorden a lo largo de la ejecución. Una excepción a esta regla es la siguiente: cuando se resuelve un valor impropio que depende de una operación de lectura, se efectúan todas las operaciones pendientes de E/S hasta ese instante, y se _inhabilitan_ las operaciones de lectura hacia el pasado.

## Paradojas temporales

La asignación de una variable corresponde a la mutación del valor contenido en una celda.
Los viajes en el tiempo pueden afectar parcialmente el flujo de control:
```
def main():
  flag = True
  t = now()
  if flag:
    print("elefante")
  else:
    print("mariposa")
  at t:
    flag = False

main() # Imprime mariposa
```
Sin embargo, si el flujo de control depende de un valor y se lo trata de modificar,
esto conduce a una paradoja temporal:
```
def main():
  flag = True
  t = now()
  if flag:
    at t:
      flag = False

main() # ERROR: Assignment would lead to time paradox.
```

También se produce una paradoja temporal si se trata de modificar un valor que
depende de sí mismo:
```
def main():
  x = 1
  t = now()
  at t:
    x = $x + 1
  print(x)

main() # ERROR: Time paradox: value depends on itself.
```

Sin embargo, sí es legítimo producir una estructura de datos cíclica:
```
def main():
  x = 1
  t = now()
  at t:
    x = [$x, $x]
  print(len(x))

main() # Imprime 2
       # (El valor final de x es un árbol binario infinito)
```

## Inversión de tiempos

Una característica interesante de Mariposa es que permite manipular instantes
dentro de estructuras de datos.
En el siguiente ejemplo se crea una lista de instantes
`[now(), now(), now(), now(), now()]`.
Los instantes están ordenados cronológicamente porque la evaluación
en Mariposa procede de izquierda a derecha.
Se invierte la lista, y se produce un efecto en cada uno de los instantes
contenidos en la lista.
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
Esto produce como resultado:
```
4
3
2
1
0
```

## La técnica del valor futuro (FutureValue)

El siguiente programa imprime `1`:
```
def main():
  x = 0
  z = (now(), x)
  print(z[1])
  at z[0]:
	x = 1

main() # Imprime 1
```
En este programa `z` es una tupla que contiene un instante y
un valor (ténicamente, un valor _impropio_), que corresponde al valor que se encuentre
en la dirección de memoria a la que esté ligada la variable `x`.
Es importante notar que el programa depende del hecho de que la evaluación en
Mariposa procede de izquierda a derecha.

Esta idea puede generalizarse a una técnica que permite crear un "valor futuro" `f`
con una operación `get(f)` que devuelve el valor que le será dado a `f` en algún
momento del futuro y una operación `set(f, x)` que le otorga el valor `x` a `f`:

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

main() # Imprime 1
```
En el programa de arriba, se crea un valor futuro `f`, se muestra su valor futuro
y recién entonces se le otorga el valor `1`.
Esta técnica está limitada a otorgarle valor a lo sumo una vez. Es decir,
si se invoca más de una vez a `set(f, ...)` se obtendrá el error
`Multiple travelers to single point in time`.

## Encadenamiento de tiempos

No es posible viajar más de una vez al mismo instante en el tiempo.
Sin embargo, es posible hacer un viaje en el tiempo al instante `t`
para hacer ciertas acciones
pero _además_ aprovechar el viaje
para modificar el valor de `t` en el marco de tiempo padre,
como quien encadena puntos en un tejido,
para que refiera a un nuevo instante que podrá ser el destino de futuros
viajes.

Por ejemplo, el siguiente programa falla porque pretende viajar más de una
vez al mismo instante:

```
def main():
  t = now()
  for i in range(10):
	at t:
	  print($i)

main() # ERROR: Multiple travelers to single point in time.
```

En cambio, el siguiente programa funciona, usando la técnica de encadenamiento:

```
def main():
  t = now()
  for i in range(10):
	at t:
	  print($i)
	  $t = now()

main() # Imprime 0 1 ... 9
```

Un hecho curioso es que el mismo encadenamiento puede hacerse _hacia el pasado_:

```
def main():
  t = now()
  for i in range(10):
    at t:
      $t = now()
      print($i)

main() # Imprime 9 ... 1 0
```

Usando la técnica de encadenamiento, se puede definir una versión mejorada de la
técnica de valores futuros, que permite invocar a `set(f, ...)` un número indefinido
de veces:

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

main() # Imprime 9
```

## Características implementadas (con bugs)

* Primitivas para viajes en el tiempo: `now()`, `at..` y `$(...)`.
* `def` con número fijo de parámetros.
* `print` e `input`.
* Booleanos (`True`, `False`) con operaciones lógicas (`and`, `or`, `not`).
* Números enteros con operaciones aritméticas (`+`, `-`, `*`, `/`, `%`, `**`). La división es entera.
* Operadores relacionales (`==`, `!=`, `<`, `<=`, `>`, `>=`).
* Strings de simple y multi-línea con escapes de caracteres especiales y hexadecimales.
* Strings, tuplas inmutables y listas mutables implementan `len` e indexación, con índices posiblemente negativos.
* Asignación _destructuring_ cuyo LHS puede incluir tuplas y listas (e.g. `x, y = y, x`).
* `if..elif..elif..else..`
* `while`
* `for..in..` sobre strings, tuplas y listas. Se traduce a un `while` por medio del proceso de _desugaring_.
* `range(n)` con un único parámetro. Devuelve una lista como en el viejo Python 2.

## Características pendientes o no implementadas

* `return`, `break`, `continue`: habría que reescribir el intérprete en CPS.
* `at` al nivel de las expresiones, `x@t`
* `__getitem__` y `__setitem__` actualmente fuerzan la resolución de los valores que toman como argumento, podrían hacerse _lazy_.
* `range` con más de un argumento.
* El `if` y el `while` actualmente no se comportan de manera uniforme. El `if` se posterga sin forzar la resolución de la condición, en tanto que el `while` siempre fuerza la resolución de la condición. Esto es arbitrario, y podría hacerse de otras maneras.
* Diccionarios y conjuntos.
* Clases, objetos,
* Notaciones `x += 1`, `x -= 1`, etc.
* Feteado de listas (_slices_).
* Listas por comprensión.
* Generators.
* Archivos.
* Excepciones.
* Números de punto flotante.
* Operaciones _bitwise_.
* Escapes decimales y octales en strings.
* Módulos.
* (Etcétera).

## Semántica y corrección de la implementación

No es evidente cómo se le podría dar una semántica formal al lenguaje Mariposa.
Dado que no hay una especificación de su semántica,
ni siquiera tiene sentido preguntarse si la implementación es correcta o incorrecta.
Sin embargo, estamos seguros de que la implementación es incorrecta para casi cualquier
semántica concebible. Debe entenderse sólo como un juego exploratorio.

