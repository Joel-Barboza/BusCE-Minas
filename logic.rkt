#lang racket

(provide crear-matriz vecinos crear-casilla
         obtener-casilla obtener-mina obtener-bandera obtener-vecinos-casilla
         obtener-indice obtener-coordenadas crear-matriz-con-bombas)

;; ==================== FUNCIONES DE MATRIZ ====================
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

;; crea el elemento de la matriz que representa una casilla
(define (crear-casilla mina bandera casilla filas columnas)
  (list mina bandera (vecinos casilla filas columnas)))

(define (crear-matriz filas columnas)
  ;; función auxiliar para construir una fila
  (define (crear-fila idx restantes)
    (cond
      ((zero? restantes) '())  ;; fila completa
      (else
       (cons (crear-casilla 0 0 idx filas columnas)
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


(define (crear-lista-bombas filas columnas dificultad )
  (define casillas (* filas columnas))
  (define bombas (cantidad-bombas filas columnas dificultad))
  (define (bombas-aux idx lista-bombas)
    (cond ((zero? idx) lista-bombas)
          (else
           (bombas-aux (- idx 1) (cons (+ 1 (random (* filas columnas))) lista-bombas)))))
  (bombas-aux bombas '()))

(define (cantidad-bombas filas columnas dificultad)
  (cond ((equal? dificultad "Fácil")
         (round (/ (* filas columnas) 10)))
        ((equal? dificultad "Medio")
         (round (/ (* filas columnas) 15)))
        ((equal? dificultad "Difícil")
         (round (/ (* filas columnas) 20)))
        ))


;; ==================== FUNCIONES AUXILIARES ====================
(define (obtener-casilla matriz i j)
  (list-ref (list-ref matriz i) j))

(define (obtener-mina casilla)
  (list-ref casilla 0))

(define (obtener-bandera casilla)
  (list-ref casilla 1))

(define (obtener-vecinos-casilla casilla)
  (list-ref casilla 2))

(define (obtener-indice filas columnas i j)
  (+ (* i columnas) j 1))

(define (obtener-coordenadas idx columnas)
  (define i (quotient (- idx 1) columnas))
  (define j (remainder (- idx 1) columnas))
  (list i j))






#|(define (actualizar-casilla elem)
  (cons 1 (cdr elem)))

(define (reemplazar-en-fila matriz idx elem columnas)
  (define coords (obtener-coordenadas idx columnas))
  (define (fila-aux i matriz resultado )
    (cond ((equal? i (car coords))
           (columna-aux 0 (car matriz) '() (reverse resultado) (cdr matriz)))
          (else
           (fila-aux (+ i 1) (cdr matriz) (cons (car matriz) (reverse resultado))))))
  (define (columna-aux j fila result-fila anterior posterior)
    (cond ((equal? j (cadr coords))
           ;;(reverse result-fila))
           (append anterior
                   (list (append result-fila (list (actualizar-casilla (car fila))) (cdr fila)))
                   posterior))
           ;;(#t))
          (else
           (columna-aux (+ j 1) (cdr fila) (cons (car fila) (reverse result-fila)) anterior posterior))))
  (fila-aux 0 matriz '()))|#
;;(print (reemplazar-en-fila 10 '() 4))

(define (actualizar-casilla elem)
  (cons 1 (cdr elem)))

;; reemplaza una casilla en la fila j
(define (reemplazar-en-fila fila j)
  (cond
    [(zero? j) (cons (actualizar-casilla (car fila)) (cdr fila))]
    [else (cons (car fila) (reemplazar-en-fila (cdr fila) (sub1 j)))]))

;; reemplaza una casilla en la matriz (fila i, col j)
(define (reemplazar-en-matriz matriz i j)
  (cond
    [(zero? i) (cons (reemplazar-en-fila (car matriz) j) (cdr matriz))]
    [else (cons (car matriz) (reemplazar-en-matriz (cdr matriz) (sub1 i) j))]))

(define (reemplazar-casilla matriz idx columnas)
  (define coords (obtener-coordenadas idx columnas))
  (define i (first coords))
  (define j (second coords))
  (reemplazar-en-matriz matriz i j))

(define (matriz-con-bombas matriz lista-bombas columnas)
  (cond
    [(null? lista-bombas) matriz]
    [else
     (matriz-con-bombas
      (reemplazar-casilla matriz (car lista-bombas) columnas)
      (cdr lista-bombas)
      columnas)]))


;;(define A (crear-matriz 4 4))
;;(print A)
;;(define v (crear-lista-bombas 4 4 "Fácil"))
;;(print v)

#|(define (matriz-con-bombas matriz lista-bombas columnas)
  (define (colocar-bomba mat lista)
    (cond ((null? lista)
           mat)
          (else
           (colocar-bomba (reemplazar-en-fila matriz (car lista) '() columnas) (cdr lista) ))))
  (colocar-bomba matriz lista-bombas))|#

;;(matriz-con-bombas A v 4)

#|(define (crear-matriz-con-bombas columnas filas dificultad)
  (define A (crear-matriz filas columnas))
  (define v (crear-lista-bombas filas columnas "Fácil"))
  (define Av (matriz-con-bombas A v columnas))
  (print Av)
  Av)|#


(define (crear-matriz-con-bombas filas columnas dificultad)
  (define A (crear-matriz filas columnas))
  (define v (crear-lista-bombas filas columnas dificultad))
  (define Av (matriz-con-bombas A v columnas))
  (print Av) ; opcional, solo para debug
  Av)




