# mariposa

Mariposa es un lenguaje de programación de juguete minimalista inspirado en Python destinado a explorar la posibilidad de los programas tengan la capacidad de "viajar en el tiempo".

Un ejemplo básico es el siguiente programa:
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
El resultado de ejecutar el programa es así:
```
$ mariposa examples/01.m
2
```
