#lang racket

(provide crear-matriz)

(define matriz '())

(define (crear-matriz filas columnas dificultad)
  (printf "Vecinos de casilla ~a: ~a" columnas (vecinos columnas filas columnas '() 8))
  )

#|
Contador inicia en 8 siempre, ya que se alrededor de cada casilla pueden haber hasta 8 vecinos
Los calcula de manera horaria, iniciando por el superior izquierdo
|#
(define (vecinos casilla filas columnas lista contador)
  (cond ((zero? contador) lista)
        ;; superior izquierdo
        ((and (not (equal? (- (remainder casilla columnas) 1) 0)) (equal? contador 1) (> (- casilla columnas 1) 0))
         (vecinos casilla filas columnas (cons (- casilla columnas 1) lista) (- contador 1)))

        ;; superior
        ((and (> casilla columnas) (equal? contador 2))
         (vecinos casilla filas columnas (cons (- casilla columnas) lista) (- contador 1)))

        ;; superior derecho
        ((and (not (equal? (remainder casilla columnas) 0)) (equal? contador 3) (> (- casilla columnas) 0))
         (vecinos casilla filas columnas (cons (- casilla columnas -1) lista) (- contador 1)))

        ;; izquierdo
        ((and (not (equal? (- (remainder casilla columnas) 1) 0)) (equal? contador 4) )
         (vecinos casilla filas columnas (cons (- casilla 1) lista) (- contador 1)))

        ;; derecho
        ((and (not (equal? (remainder casilla columnas) 0)) (equal? contador 5) )
         (vecinos casilla filas columnas (cons (+ casilla 1) lista) (- contador 1)))

        ;;inferior izquierdo
        ((and (not (equal? (- (remainder casilla columnas) 1) 0)) (equal? contador 6) (<= (+ casilla columnas) (* columnas filas)))
         (vecinos casilla filas columnas (cons (+ casilla columnas -1) lista) (- contador 1)))

        ;; inferior
        ((and (<= casilla (- ( * columnas filas) columnas)) (equal? contador 7))
         (vecinos casilla filas columnas (cons (+ casilla columnas) lista) (- contador 1)))

        ;; inferior derecho
        ((and (not (equal? (remainder casilla columnas) 0)) (equal? contador 8) (<= (+ casilla columnas 1) (* columnas filas)))
         (vecinos casilla filas columnas (cons (+ casilla columnas 1) lista) (- contador 1)))
        (else (vecinos casilla filas columnas lista (- contador 1))))
)


#| Pruebas para cuadricula 4x4
(vecinos 1 4 4 '() 8)
(vecinos 2 4 4 '() 8)
(vecinos 3 4 4 '() 8)
(vecinos 4 4 4 '() 8)
(vecinos 5 4 4 '() 8)
(vecinos 6 4 4 '() 8)
(vecinos 7 4 4 '() 8)
(vecinos 8 4 4 '() 8)
(vecinos 9 4 4 '() 8)
(vecinos 10 4 4 '() 8)
(vecinos 11 4 4 '() 8)
(vecinos 12 4 4 '() 8)
(vecinos 13 4 4 '() 8)
(vecinos 14 4 4 '() 8)
(vecinos 15 4 4 '() 8)
(vecinos 16 4 4 '() 8)|#

