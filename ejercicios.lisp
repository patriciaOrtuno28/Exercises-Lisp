; Ejercicio 1
;; Escribir una función FIRSTP que devuelve true si el primer objeto pasado a la función coincide con el primer valor de la lista pasada como segundo parámetro a la función
(DEFUN FIRSTP (A L)
    (COND
        ((EQ NIL L) NIL)
        (T (EQUAL A (CAR L)))
    )
)

(write-line "Resultado: ")
(write (FIRSTP 'A '(A B C))) ; T
(write-line "")
(write (FIRSTP 'MARTES '(LUNES MARTES MIERCOLES))) ; NIL
(write-line "")

; Ejercicio 2
;; Escribir una función DUPLICAR que toma como argumento una lista y devuelve la lista cuyos elementos son pares (lista de dos elementos) compuestos por los elementos de la primera. Tenga en cuenta que los elementos de la lista resultante son, a su vez, listas de dos elementos

(DEFUN DUPLICAR (L)
    (COND
        ((EQ NIL (CAR L)) NIL)
        (T (CONS (LIST (CAR L) (CAR L)) (DUPLICAR(CDR L))))
    )
)

(write-line "Resultado: ")
(write (DUPLICAR '(A B C)))
(write-line "")

; Ejercicio 3
;; Escribir una función recursiva COUNTDOWN que toma como argumento un número N positivo y genera una lista de enteros desde N hasta 1. Si se le pasa un número negativo o cero, debe devolver la lista vacia (NIL).

(DEFUN COUNTDOWN (N)
    (COND
        ((< N 1) NIL)
        (T (CONS N (COUNTDOWN(- N 1))))
    )
)

(write-line "Resultado: ")
(write (COUNTDOWN 10))
(write-line "")

; Ejercicio 4
;; Escribir una función REVERSE que toma como argumento una lista y devuelve la lista invertida, es decir, con los elementos en el orden inverso que aparecen en el original.

(DEFUN MY-REVERSE (L)
    (COND
        ((EQ NIL L) NIL)
        (T (APPEND (MY-REVERSE (CDR L)) (LIST (CAR L))))
    )
)

(write-line "Resultado: ")
(write (MY-REVERSE '(A B C)))
(write-line "")

; Ejercicio 5
;; Escribir una función recursiva SUBSTITUTE que toma tres argumentos, los dos primeros son símbolos X e Y y el tercero una lista L. La función construye una nueva lista en la que todas las apariciones del elemento Y es sustituido por X.

(DEFUN SUBSTITUTE2 (X Y L)
    (COND
        ((EQ NIL L) NIL)
        ((EQ X Y) L)
        ((EQ Y (CAR L)) (CONS X (SUBSTITUTE X Y (CDR L))))
        (T (CONS (CAR L) (SUBSTITUTE X Y (CDR L))))
    )
)

(write-line "Resultado: ")
(write (SUBSTITUTE2 'A 'B '(A B C)))
(write-line "")

; Ejercicio 6
;; Escribir un predicado SETEQUAL que tome dos listas como argumentos y que devuelva T si todos los elementos de la primera lista estan incluidos en la segunda y a la inversa. En caso contrario debe devolver NIL.

(DEFUN INCLUDES (L1 L2) 
   (COND
      ((EQ NIL L1) T) ; Lista vacía
      ((MEMBER (CAR L1) L2)(INCLUDES (CDR L1) L2)) 
      (T ())))
      
(DEFUN SETEQUAL (L1 L2)
    (IF (AND (INCLUDES L1 L2) (INCLUDES L2 L1))
      T
      NIL
    ) 
)

(write-line "Resultado: ")
(write (SETEQUAL '(B C A) '(A B C)))
(write-line "")

; Ejercicio 7
;; Escribir una función IMPARES que toma como argumento una lista y devuelve una lista formada por los elementos en las posiciones impares de la lista original.

(DEFUN IMPARES (L)
    (COND
        ((EQ NIL L) ())
        (T (CONS (CAR L) (IMPARES (CDR(CDR L)))))
    ) 
)

(write-line "Resultado: ")
(write (IMPARES '(A B C D E F G H I)))
(write-line "")

; Ejercicio 8
;; Obtener el último elemento de una lista.
;;; Obtenerlo como lista.
(DEFUN MYLAST-A (L)
    (COND
        ((EQ NIL L) NIL)
        (T (LAST L))
    )
)

;;; Obtenerlo como valor
(DEFUN MYLAST-B (L)
    (COND
        ((EQ NIL L) NIL)
        (T (CAR (REVERSE L)))
    )
)

(write-line "Resultado: ")
(write (MYLAST-A '(A B C D)))
(write (MYLAST-B '(A B C D)))
(write-line "")

; Ejercicio 9
;; Eliminar el último valor de una lista.
(DEFUN WITHOUT-LAST (L)
    (COND
        ((EQ NIL L) NIL)
        (T (REVERSE (CDR (REVERSE L))))
    )
)

(write-line "Resultado: ")
(write (WITHOUT-LAST '(A B C D)))
(write-line "")

; Ejercicio 9
;; Comprobar si una lista es un palíndromo.
(DEFUN ISPALINDROME (L)
    (COND
        ((EQ NIL L) T)
        (T (AND (EQUAL (LIST(CAR L)) (LAST L)) 
            (ISPALINDROME (REVERSE (CDR (REVERSE (CDR L))))))
        )
    )
)

(write-line "Resultado: ")
(write (ISPALINDROME '(A B C C B A)))
(write-line "")

; Ejercicio 10
;; Encontrar el nth valor en una lista.
(DEFUN ELEMENT-AT (L X)
    (COND
        ((EQ NIL L) NIL)
        ((EQ X 0) (CAR L))
        (T (ELEMENT-AT (CDR L) (- X 1)))
    )
)

(write-line "Resultado: ")
(write (ELEMENT-AT '(A B C D E) 3))
(write-line "")

; Ejercicio 11
;; Find the number of elements of a list.
(DEFUN SIZEL (L)
    (COND
        ((EQ NIL L) 0)
        (T (+ 1 (SIZEL (CDR L))))
    )
)

(write-line "Resultado: ")
(write (SIZEL '(A B C D E)))
(write-line "")

; Ejercicio 12
;; Flatten a nested list structure.
(DEFUN FLATTEN (L)
    (COND
        ((EQ NIL L) NIL)
        ((LISTP (CAR L)) (APPEND (FLATTEN (CAR L)) (FLATTEN (CDR L))))
        (T (CONS (CAR L) (FLATTEN (CDR L))))
    )
)

(write-line "Resultado: ")
(write (FLATTEN '(a (b (c d) e))))
(write-line "")

; Ejercicio 12
;; Escribir una función con un objeto y una lista de entrada. Devolver una nueva lista con todos los valores de la lista original que sean mayores o iguales que el objeto de entrada.

(DEFUN SUPERA (A L)
    (COND
        ((EQ NIL L) NIL)
        ((>= (CAR L) A) (CONS (CAR L) (SUPERA A (CDR L))))
        (T (SUPERA A (CDR L)))
    )
)

(write-line "Resultado: ")
(write (SUPERA 2 '(1 2 3 4 2)))
(write-line "")