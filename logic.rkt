#lang racket

(provide crear-matriz vecinos crear-casilla
         obtener-casilla obtener-mina obtener-bandera obtener-vecinos-casilla
         obtener-indice obtener-coordenadas crear-matriz-con-bombas
         crear-lista-bombas bombas-vecinas)  ;; Nueva función exportada

;; ==================== FUNCIONES DE MATRIZ ====================
(define (vecinos casilla filas columnas)
  (define (aux indice acumulado)
    (cond
      ((<= indice 0) acumulado)

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

      (else (aux (- indice 1) acumulado))))
  (aux 8 '()))

(define (crear-casilla mina bandera casilla filas columnas)
  (list mina bandera (vecinos casilla filas columnas)))

(define (crear-matriz filas columnas)
  (define (crear-fila idx restantes)
    (cond
      ((zero? restantes) '())
      (else
       (cons (crear-casilla 0 0 idx filas columnas)
             (crear-fila (+ idx 1) (- restantes 1))))))
  
  (define (crear idx fila-restantes)
    (cond
      ((zero? fila-restantes) '())
      (else
       (cons (crear-fila idx columnas)
             (crear (+ idx columnas) (- fila-restantes 1))))))
  
  (crear 1 filas))

(define (crear-lista-bombas filas columnas dificultad)
  (define casillas (* filas columnas))
  (define bombas (cantidad-bombas filas columnas dificultad))
  (define (bombas-aux idx lista-bombas)
    (cond ((zero? idx) lista-bombas)
          (else
           (define nueva-bomba (+ 1 (random (* filas columnas))))  ;; Asegurar que empieza desde 1
           (if (member nueva-bomba lista-bombas)
               (bombas-aux idx lista-bombas)  ;; Si ya existe, intentar de nuevo
               (bombas-aux (- idx 1) (cons nueva-bomba lista-bombas))))))
  (bombas-aux bombas '()))

(define (cantidad-bombas filas columnas dificultad)
  (cond ((equal? dificultad "Fácil")
         (round (* filas columnas 0.1)))
        ((equal? dificultad "Medio")
         (round (* filas columnas 0.15)))
        ((equal? dificultad "Difícil")
         (round (* filas columnas 0.2)))))

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

(define (actualizar-casilla elem)
  (cons 1 (cdr elem)))

(define (reemplazar-en-fila fila j)
  (cond
    [(zero? j) (cons (actualizar-casilla (car fila)) (cdr fila))]
    [else (cons (car fila) (reemplazar-en-fila (cdr fila) (sub1 j)))]))

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

(define (crear-matriz-con-bombas filas columnas dificultad)
  (define A (crear-matriz filas columnas))
  (define v (crear-lista-bombas filas columnas dificultad))
  (define Av (matriz-con-bombas A v columnas))
  Av)

;;(crear-matriz-con-bombas 8 8 "Fácil")

#|(define (buscar-en-bombas lista-bombas elem)
  (define (buscar-aux contador)
    (cond ((null? lista-bombas)
           (#f))
          ((equal? (car lista-bombas) elem)
           (#t))
          (else
           (buscar-aux (cdr lista-bombas)))))
  (buscar-aux lista-bombas))|#

(define (incrementar-valores lista)
  (define (inc-aux lista resultado)
    (cond ((null? lista)
           (reverse resultado))
          (else
           (inc-aux (cdr lista) (cons (+ 1 (car lista)) resultado)))))
  (inc-aux lista '()))

(define (bombas-vecinas idx vecinos lista-bombas)
  (define lista-vecinos (incrementar-valores vecinos))
  (define (contador-aux contador lista-vecinos)
    (cond ((null? lista-vecinos)
           contador)
          ((member (car lista-vecinos) lista-bombas)
           (contador-aux (+ 1 contador) (cdr lista-vecinos)))
          (else
           (contador-aux contador (cdr lista-vecinos)))))
  (contador-aux 0 lista-vecinos))




