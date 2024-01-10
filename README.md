# Mariposa: programas que viajan en el tiempo

Mariposa es un lenguaje de programación de juguete minimalista inspirado en Python destinado a explorar la posibilidad de los programas tengan la capacidad de "viajar en el tiempo".

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
Luego se imprime el valor de `x`. En un lenguaje de programación usual, esto haría que se imprima en pantalla el valor `1`.
Sin embargo, en Mariposa, existe la posibilidad de que el programa viaje atrás en el tiempo y modifique el valor de `x`. El comando `at t: <bloque>` ejecuta el bloque de código en el instante indicado por `t`. El efecto de este comando es equivalente a modificar el valor de `x` después de la instrucción `t = now()` pero antes de la instrucción `print(x)`.
El resultado de ejecutar el programa es, entonces:
```
$ mariposa examples/01.m
2
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
  f(now())
  print(x)

main()
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
  f(now())
  print(x)

main()
```

## Viajes al futuro

El ejemplo anterior ilustra que los programas pueden viajar al pasado, usando
el comando `at t: ...`, con una variable `t` que referencia un instante del pasado.
¿Podríamos viajar también al futuro? A primera vista esto no parece posible, ya que deberíamos
contar con una variable `t_futuro` que referencie un instante del futuro.
Pero esto es posible con la siguiente técnica, que viaja al presente desde el futuro
para establecer el valor de `t_futuro`:
```
t_presente = now()
print("Estamos en el presente")
at t_futuro:
  print("Código que se ejecutará en el futuro")
print("...Pasan muchos años...")
at t_pasado:
  t_futuro = $(now())
```

