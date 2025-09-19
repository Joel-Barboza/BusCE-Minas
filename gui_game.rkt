#lang racket/gui

;;(require "gui.rkt")
(require "logic.rkt")
;; Importar la función iniciar-gui-game
;;(require "logic.rkt")

(provide obtener-filas)

(define matriz '())

(define start-frame (new frame%
                         [label "Pantalla de Inicio"]
                         [width 400]
                         [height 300]))

(define main-panel
  (new vertical-panel%
       [parent start-frame]
       [alignment '(center center)]
       [spacing 15]
       [border 20]
       [stretchable-height #t]
       [stretchable-width #t]))

;; Selección de filas
(new message% [parent main-panel] [label "Cantidad de Filas (8 - 15):"])
(define filas-choice
  (new choice%
       [parent main-panel]
       [label ""]
       [choices (map number->string (range 8 16))]))

;; Selección de columnas
(new message% [parent main-panel] [label "Cantidad de Columnas (8 - 15):"])
(define cols-choice
  (new choice%
       [parent main-panel]
       [label ""]
       [choices (map number->string (range 8 16))]))

;; Selección de dificultad
(new message% [parent main-panel] [label "Dificultad:"])
(define dificultad-choice
  (new choice%
       [parent main-panel]
       [label ""]
       [choices '("Fácil" "Medio" "Difícil")]))


(define (actualizar-matriz nueva-matriz)
  (set! matriz nueva-matriz))

;; Botón de iniciar
(new button% [parent main-panel]
             [label "Iniciar"]
             [callback
              (lambda (button event)
                (define filas (string->number (send filas-choice get-string-selection)))
                (define cols (string->number (send cols-choice get-string-selection)))
                (define dificultad (send dificultad-choice get-string-selection))
                (send start-frame show #f) ;; oculta pantalla de inicio
                (actualizar-matriz (crear-matriz filas cols dificultad))
                (iniciar-gui-game filas cols dificultad)
                ;;(crear-matriz filas cols dificultad)
                )])

(send start-frame show #t)


(define obtener-filas
  (string->number (send filas-choice get-string-selection)))

(define obtener-columnas
  (string->number (send cols-choice get-string-selection)))

(define obtener-dificultad
  (send dificultad-choice get-string-selection))

;;(define matriz (crear-matriz (obtener-filas) (obtener-columnas) (obtener-dificultad)))




;; ==================== ESTRUCTURA DEL GRAFO-MATRIZ ====================
(struct grafo-matriz (filas columnas nodos vecinos) #:mutable)

(define (crear-grafo-matriz n m)
  (define nodos (make-vector (* n m) #f))
  (define vecinos (make-hash))
  
  (for* ([i (in-range n)]
         [j (in-range m)])
    (define idx (+ (* i m) j))
    (vector-set! nodos idx (list i j 'libre)))
  
  (for* ([i (in-range n)]
         [j (in-range m)])
    (define actual (+ (* i m) j))
    (define vecinos-actual '())
    
    ;; Vecinos ortogonales (arriba, abajo, izquierda, derecha)
    (when (> i 0)
      (set! vecinos-actual (cons (+ (* (- i 1) m) j) vecinos-actual))) ; Arriba
    (when (< i (- n 1))
      (set! vecinos-actual (cons (+ (* (+ i 1) m) j) vecinos-actual))) ; Abajo
    (when (> j 0)
      (set! vecinos-actual (cons (+ (* i m) (- j 1)) vecinos-actual))) ; Izquierda
    (when (< j (- m 1))
      (set! vecinos-actual (cons (+ (* i m) (+ j 1)) vecinos-actual))) ; Derecha
    
    ;; Vecinos diagonales (las cuatro diagonales)
    (when (and (> i 0) (> j 0))
      (set! vecinos-actual (cons (+ (* (- i 1) m) (- j 1)) vecinos-actual))) ; Arriba-izquierda
    (when (and (> i 0) (< j (- m 1)))
      (set! vecinos-actual (cons (+ (* (- i 1) m) (+ j 1)) vecinos-actual))) ; Arriba-derecha
    (when (and (< i (- n 1)) (> j 0))
      (set! vecinos-actual (cons (+ (* (+ i 1) m) (- j 1)) vecinos-actual))) ; Abajo-izquierda
    (when (and (< i (- n 1)) (< j (- m 1)))
      (set! vecinos-actual (cons (+ (* (+ i 1) m) (+ j 1)) vecinos-actual))) ; Abajo-derecha
    
    (hash-set! vecinos actual vecinos-actual))
  
  (grafo-matriz n m nodos vecinos))

(define mi-grafo (crear-grafo-matriz 10 10))

(define (obtener-coordenadas grafo idx)
  (vector-ref (grafo-matriz-nodos grafo) idx))

(define (obtener-indice grafo i j)
  (+ (* i (grafo-matriz-columnas grafo)) j))

(define (obtener-vecinos grafo idx)
  (hash-ref (grafo-matriz-vecinos grafo) idx '()))

;; ==================== INTERFAZ GRÁFICA ====================
(define frame (new frame%
                   [label "Matriz 10x10 - Selector de Celdas"]
                   [width 800]
                   [height 650]))

;; Panel principal
(define panel-principal (new vertical-panel%
                            [parent frame]
                            [alignment '(center center)]
                            [spacing 10]
                            [min-width 700]))

;; Panel para la matriz
(define panel-matriz-container (new group-box-panel%
                                   [parent panel-principal]
                                   [label "Matriz 10x10"]
                                   [min-width 500]
                                   [min-height 400]))

(define panel-matriz (new vertical-panel%
                         [parent panel-matriz-container]
                         [alignment '(center center)]))

;; Crear 10 filas con 10 botones cada una
(define botones-matriz (make-vector 10))
(for ([i (in-range 10)])
  (define fila (make-vector 10))
  (define panel-fila (new horizontal-panel%
                         [parent panel-matriz]
                         [alignment '(center center)]
                         [spacing 2]))
  
  (for ([j (in-range 10)])
    (define idx (+ (* i 10) j))
    (define boton (new button%
                      [parent panel-fila]
                      [label (number->string idx)]
                      [min-width 40]
                      [min-height 40]
                      [callback (λ (b e) (mostrar-info-celda i j))]))
    (vector-set! fila j boton))
  (vector-set! botones-matriz i fila))

;; Etiqueta de información
(define info-label (new message%
                       [parent panel-principal]
                       [label "Selecciona una celda para ver su información"]
                       [auto-resize #t]
                       [min-width 600]
                       [stretchable-width #t]))

;; Panel de control
(define panel-control (new horizontal-panel%
                          [parent panel-principal]
                          [alignment '(center center)]
                          [spacing 10]))

(define boton-limpiar (new button%
                          [parent panel-control]
                          [label "Limpiar Selección"]
                          [callback (λ (b e) (limpiar-seleccion))]))

(define boton-mostrar-todo (new button%
                               [parent panel-control]
                               [label "Mostrar Todos los Datos"]
                               [callback (λ (b e) (mostrar-todos-datos))]))

;; Panel de búsqueda
(define panel-busqueda (new horizontal-panel%
                           [parent panel-principal]
                           [alignment '(center center)]
                           [spacing 5]))

(define campo-busqueda (new text-field%
                           [parent panel-busqueda]
                           [label "Buscar índice (0-99):"]
                           [init-value ""]
                           [min-width 120]))

(define boton-buscar (new button%
                         [parent panel-busqueda]
                         [label "Buscar"]
                         [callback (λ (b e)
                                    (define texto (send campo-busqueda get-value))
                                    (when (non-empty-string? texto)
                                      (buscar-por-indice (string->number texto))))]))

;; Variables para controlar la selección
(define celda-seleccionada #f)
(define boton-seleccionado #f)

;; Crear estilos de fuente
(define fuente-normal (make-object font% 10 'default 'normal 'normal))
(define fuente-seleccionada (make-object font% 12 'default 'normal 'bold))

;; Función para mostrar información de la celda seleccionada
(define (mostrar-info-celda i j)
  (define idx (obtener-indice mi-grafo i j))
  (define coord (obtener-coordenadas mi-grafo idx))
  (define vecinos (obtener-vecinos mi-grafo idx))
  
  ;; Limpiar selección anterior
  (when boton-seleccionado
    (send boton-seleccionado set-label 
          (number->string (obtener-indice mi-grafo (car celda-seleccionada) (cdr celda-seleccionada))))
    (send boton-seleccionado enable #t))
  
  ;; Marcar nueva selección
  (set! celda-seleccionada (cons i j))
  (set! boton-seleccionado (vector-ref (vector-ref botones-matriz i) j))
  (send boton-seleccionado set-label "X")
  (send boton-seleccionado enable #f)  ; Deshabilitar para resaltar
  
  ;; Mostrar información detallada
  (define info-texto
    (string-append
     "=== CELDA SELECCIONADA ===\n"
     "Coordenadas: (" (number->string i) ", " (number->string j) ")\n"
     "Índice: " (number->string idx) "\n"
     "Número de vecinos: " (number->string (length vecinos)) "\n"
     "Vecinos: " (string-join (map number->string (sort vecinos <)) ", ")))
  
  (send info-label set-label info-texto))

;; Función para limpiar la selección
(define (limpiar-seleccion)
  (when boton-seleccionado
    (send boton-seleccionado set-label 
          (number->string (obtener-indice mi-grafo (car celda-seleccionada) (cdr celda-seleccionada))))
    (send boton-seleccionado enable #t))
  (set! celda-seleccionada #f)
  (set! boton-seleccionado #f)
  (send info-label set-label "Selecciona una celda para ver su información"))

;; Función para mostrar todos los datos de la matriz
(define (mostrar-todos-datos)
  (define ventana-datos (new frame%
                            [label "Datos Completos de la Matriz"]
                            [width 500]
                            [height 400]))
  
  (define texto-datos (new text%))
  (define editor-datos (new editor-canvas%
                           [parent ventana-datos]
                           [editor texto-datos]
                           [min-width 480]
                           [min-height 350]))
  
  (send texto-datos insert "=== MATRIZ 10x10 - DATOS COMPLETOS ===\n\n")
  
  (for* ([i (in-range 10)]
         [j (in-range 10)])
    (define idx (obtener-indice mi-grafo i j))
    (define coord (obtener-coordenadas mi-grafo idx))
    (define vecinos (obtener-vecinos mi-grafo idx))
    
    (send texto-datos insert 
          (format "Celda [~a,~a]: Índice ~a\n" i j idx))
    (send texto-datos insert 
          (format "  Vecinos: ~a\n\n" (sort vecinos <))))
  
  (send ventana-datos show #t))

;; Función para buscar por índice
(define (buscar-por-indice idx)
  (when (and (number? idx) (>= idx 0) (< idx 100))
    (define i (quotient idx 10))
    (define j (remainder idx 10))
    (mostrar-info-celda i j)
    (send info-label set-label 
          (string-append (send info-label get-label)
                         "\n\n¡Búsqueda exitosa! Celda encontrada."))
    #t)
  #f)

;; Función auxiliar para verificar strings no vacíos
(define (non-empty-string? str)
  (not (string=? (string-trim str) "")))



;; Mostrar la ventana principal
;; (send frame show #t)

;; Mensaje inicial
(printf "Aplicación de matriz 10x10 iniciada!\n")
(printf "Puedes seleccionar celdas haciendo clic en los botones.\n")


(provide iniciar-gui-game)

(define (iniciar-gui-game filas cols dificultad)
  (printf "Iniciando juego con ~a filas, ~a columnas, dificultad: ~a\n"
          filas cols dificultad)
  ;; Aquí podrías regenerar el grafo en función de filas/columnas
  ;; (define mi-grafo (crear-grafo-matriz filas cols))
  ;; Por ahora, solo usamos el grafo fijo 10x10 que ya tienes
  (send frame show #t))