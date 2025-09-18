#lang racket

(provide crear-matriz)
;;(require "gui.rkt")

#|(define (d r t)
  (define y (+ r t)))
(define x (d 1 2))
(print x)|#


#|
Contador inicia en 8 siempre, ya que se alrededor de cada casilla pueden haber hasta 8 vecinos
Los calcula de manera horaria, iniciando por el superior izquierdo
|#
;;(print obtener-filas)
;;(print obtener-filas)

(define (vecinos casilla filas columnas)
  (define (aux indice acumulado)
    (cond
      ((<= indice 0) acumulado) ; se revisaron las 8 posiciones posibles

      ;; 1: superior izquierdo
      ((and (= indice 1)
            (not (= (remainder casilla columnas) 1))
            (> (- casilla columnas 1) 0))
       (aux (- indice 1) (cons (- casilla columnas 1) acumulado)))

      ;; 2: superior
      ((and (= indice 2)
            (> casilla columnas))
       (aux (- indice 1) (cons (- casilla columnas) acumulado)))

      ;; 3: superior derecho
      ((and (= indice 3)
            (not (= (remainder casilla columnas) 0))
            (> (- casilla columnas -1) 0))
       (aux (- indice 1) (cons (- casilla columnas -1) acumulado)))

      ;; 4: izquierdo
      ((and (= indice 4)
            (not (= (remainder casilla columnas) 1)))
       (aux (- indice 1) (cons (- casilla 1) acumulado)))

      ;; 5: derecho
      ((and (= indice 5)
            (not (= (remainder casilla columnas) 0)))
       (aux (- indice 1) (cons (+ casilla 1) acumulado)))

      ;; 6: inferior izquierdo
      ((and (= indice 6)
            (not (= (remainder casilla columnas) 1))
            (<= (+ casilla columnas -1) (* filas columnas)))
       (aux (- indice 1) (cons (+ casilla columnas -1) acumulado)))

      ;; 7: inferior
      ((and (= indice 7)
            (<= casilla (- (* filas columnas) columnas)))
       (aux (- indice 1) (cons (+ casilla columnas) acumulado)))

      ;; 8: inferior derecho
      ((and (= indice 8)
            (not (= (remainder casilla columnas) 0))
            (<= (+ casilla columnas 1) (* filas columnas)))
       (aux (- indice 1) (cons (+ casilla columnas 1) acumulado)))

      ;; si no se cumple la condición del índice actual → seguir al siguiente
      (else (aux (- indice 1) acumulado))))
  (aux 8 '())) ; inicializa índice en 1


(define (crear-matriz filas columnas dificultad)
  ;;(define ())
  ;; función auxiliar para construir una fila
  (define (crear-fila idx restantes)
    (cond
      ((zero? restantes) '())  ;; fila completa
      (else
       ( cons (crear-casilla 0 0 idx filas columnas)
             (crear-fila (+ idx 1) (- restantes 1))))))
  
  ;; función auxiliar para construir todas las filas
  (define (crear idx fila-restantes)
    (cond
      ((zero? fila-restantes) '())  ;; todas las filas listas
      (else
       (cons (crear-fila idx columnas)
             (crear (+ idx columnas) (- fila-restantes 1))))))
  
  ;; arrancamos desde el índice 1 y con todas las filas
  (crear 1 filas))


;; crea el elemento de la matriz que representa una casilla
(define (crear-casilla mina bandera casilla filas columnas)
  (cons mina (cons bandera (cons (vecinos casilla filas columnas) '()))))

(define (poner-mina casilla)(#t))

(crear-casilla 1 0 3 5 4)

(define matriz (crear-matriz 4 4 'facil))
(print matriz)





#| Pruebas para cuadricula 4x4
(vecinos 1 4 4 '())
(vecinos 2 4 4 '())
(vecinos 3 4 4 '())
(vecinos 4 4 4 '())
(vecinos 5 4 4 '())
(vecinos 6 4 4 '())
(vecinos 7 4 4 '())
(vecinos 8 4 4 '())
(vecinos 9 4 4 '())
(vecinos 10 4 4 '())
(vecinos 11 4 4 '())
(vecinos 12 4 4 '())
(vecinos 13 4 4 '()
(vecinos 14 4 4 '())
(vecinos 15 4 4 '())
(vecinos 16 4 4 '())|#



