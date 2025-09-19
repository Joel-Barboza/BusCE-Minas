#lang racket

(provide crear-matriz vecinos crear-casilla
         obtener-casilla obtener-mina obtener-bandera obtener-vecinos-casilla
         obtener-indice obtener-coordenadas)

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

(define (crear-matriz filas columnas dificultad)
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

(crear-lista-bombas 8 8 "Fácil")
(define (matriz-con-bombas matriz lista-bombas)
  (#t))

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



