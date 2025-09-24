#lang racket/gui

(require "logic.rkt")

(provide obtener-filas)

(define matriz '())

;; ==================== PANTALLA DE INICIO ====================
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
                (send start-frame show #f)
                (actualizar-matriz (crear-matriz filas cols))
                (iniciar-gui-game filas cols dificultad))])

(send start-frame show #t)

(define obtener-filas
  (string->number (send filas-choice get-string-selection)))

(define obtener-columnas
  (string->number (send cols-choice get-string-selection)))

(define obtener-dificultad
  (send dificultad-choice get-string-selection))

;; ==================== ESTRUCTURA DE la MATRIZ ====================
(struct grafo-matriz (filas columnas nodos vecinos) #:mutable)

(define (crear-grafo-matriz n m)
  (define nodos (make-vector (* n m) #f))
  (define vecinos (make-hash))
  
  (for* ([i (in-range n)]
         [j (in-range m)])
    (define idx (+ (* i m) j))
    (vector-set! nodos idx (list (+ i 1) (+ j 1) 'libre)))  ;; Cambio: +1 para empezar desde 1
  
  (for* ([i (in-range n)]
         [j (in-range m)])
    (define actual (+ (* i m) j))
    (define vecinos-actual '())
    
    ;; Vecinos ortogonales
    (when (> i 0)
      (set! vecinos-actual (cons (+ (* (- i 1) m) j) vecinos-actual)))
    (when (< i (- n 1))
      (set! vecinos-actual (cons (+ (* (+ i 1) m) j) vecinos-actual)))
    (when (> j 0)
      (set! vecinos-actual (cons (+ (* i m) (- j 1)) vecinos-actual)))
    (when (< j (- m 1))
      (set! vecinos-actual (cons (+ (* i m) (+ j 1)) vecinos-actual)))
    
    ;; Vecinos diagonales
    (when (and (> i 0) (> j 0))
      (set! vecinos-actual (cons (+ (* (- i 1) m) (- j 1)) vecinos-actual)))
    (when (and (> i 0) (< j (- m 1)))
      (set! vecinos-actual (cons (+ (* (- i 1) m) (+ j 1)) vecinos-actual)))
    (when (and (< i (- n 1)) (> j 0))
      (set! vecinos-actual (cons (+ (* (+ i 1) m) (- j 1)) vecinos-actual)))
    (when (and (< i (- n 1)) (< j (- m 1)))
      (set! vecinos-actual (cons (+ (* (+ i 1) m) (+ j 1)) vecinos-actual)))
    
    (hash-set! vecinos actual vecinos-actual))
  
  (grafo-matriz n m nodos vecinos))

;; Variables globales para el juego actual
(define mi-grafo #f)
(define frame #f)
(define botones-matriz #f)
(define info-label #f)
(define celda-seleccionada #f)
(define boton-seleccionado #f)
(define lista-bombas '())  ;; Nueva variable global para almacenar las bombas
(define juego-activo #t)   ;; Variable para controlar si el juego está activo

;; ==================== FUNCIONES DE LA INTERFAZ ====================
(define (crear-interfaz-matriz filas cols)
  ;; Destruir frame anterior si existe
  (when frame
    (send frame show #f)
    (send frame dispose))
  
  (set! frame (new frame%
                   [label (format "Buscaminas ~ax~a" filas cols)]
                   [width (min 800 (* cols 45))]
                   [height (min 650 (* filas 45))]))
  
  ;; Panel principal
  (define panel-principal (new vertical-panel%
                              [parent frame]
                              [alignment '(center center)]
                              [spacing 10]
                              [min-width 700]))
  
  ;; Panel para la matriz
  (define panel-matriz-container (new group-box-panel%
                                     [parent panel-principal]
                                     [label (format "Matriz ~ax~a" filas cols)]
                                     [min-width (* cols 45)]
                                     [min-height (* filas 45)]))
  
  (define panel-matriz (new vertical-panel%
                           [parent panel-matriz-container]
                           [alignment '(center center)]))
  
  ;; Crear matriz de botones dinámica (botones vacíos)
  (set! botones-matriz (make-vector filas))
  (for ([i (in-range filas)])
    (define fila (make-vector cols))
    (define panel-fila (new horizontal-panel%
                           [parent panel-matriz]
                           [alignment '(center center)]
                           [spacing 2]))
    
    (for ([j (in-range cols)])
      (define idx (+ (* i cols) j))
      (define boton (new button%
                        [parent panel-fila]
                        [label " "]  ;; Cambio: botones vacíos inicialmente
                        [min-width 40]
                        [min-height 40]
                        [callback (λ (b e) (manejar-click-celda i j))]))
      (vector-set! fila j boton))
    (vector-set! botones-matriz i fila))
  
  ;; Etiqueta de información
  (set! info-label (new message%
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
  
 #| (new button% [parent panel-control]
               [label "Limpiar Selección"]
               [callback (λ (b e) (limpiar-seleccion))])
  
  (new button% [parent panel-control]
               [label "Mostrar Todos los Datos"]
               [callback (λ (b e) (mostrar-todos-datos filas cols))])   deshabilite esto porque me parece que queda feo, pero si lo ocupa para hacer pruebas, solo quita el comentario|#
  

  
  ;; Panel de búsqueda
  (define panel-busqueda (new horizontal-panel%
                             [parent panel-principal]
                             [alignment '(center center)]
                             [spacing 5]))
  
  (define campo-busqueda (new text-field%
                             [parent panel-busqueda]
                             [label (format "Buscar índice (1-~a):" (* filas cols))]  ;; Cambio: empieza desde 1
                             [init-value ""]
                             [min-width 150]))
  
  (new button% [parent panel-busqueda]
               [label "Buscar"]
               [callback (λ (b e)
                          (define texto (send campo-busqueda get-value))
                          (when (non-empty-string? texto)
                            (buscar-por-indice (string->number texto) filas cols)))]))

(define (manejar-click-celda i j)
  (when juego-activo
    (define idx (obtener-indice mi-grafo i j))
    (define indice-real (+ idx 1))  ;; Convertir a índice que empieza desde 1
    (define tiene-bomba (member indice-real lista-bombas))
    
    (if tiene-bomba
        (perder-juego i j)
        (mostrar-info-celda i j))))

(define (perder-juego i j)
  (set! juego-activo #f)
  

  
  ;; Deshabilitar todos los botones
  (for ([fila (in-vector botones-matriz)])
    (for ([boton (in-vector fila)])
      (send boton enable #f)))
  
  ;; Mostrar mensaje de derrota
  (send info-label set-label "¡PERDISTE! Has pisado una bomba. El juego ha terminado.")
  
  ;; Mostrar ventana de mensaje
  (message-box "Game Over" 
               "¡PERDISTE! Has pisado una bomba.\nEl juego ha terminado." 
               frame 
               '(ok stop)))

(define (obtener-coordenadas grafo idx)
  (vector-ref (grafo-matriz-nodos grafo) idx))

(define (obtener-indice grafo i j)
  (+ (* i (grafo-matriz-columnas grafo)) j))

(define (obtener-vecinos grafo idx)
  (hash-ref (grafo-matriz-vecinos grafo) idx '()))

(define (mostrar-info-celda i j)
  (when mi-grafo
    (define idx (obtener-indice mi-grafo i j))
    (define coord (obtener-coordenadas mi-grafo idx))
    (define vecinos (obtener-vecinos mi-grafo idx))
    (define i-real (+ i 1))  ;; Cambio: convertir a coordenadas que empiezan desde 1
    (define j-real (+ j 1))
    
    ;; Limpiar selección anterior
    (when boton-seleccionado
      (send boton-seleccionado set-label " ")  ;; Cambio: volver a dejar vacío
      (send boton-seleccionado enable #t))
    
    ;; Marcar nueva selección
    (set! celda-seleccionada (cons i-real j-real))
    (set! boton-seleccionado (vector-ref (vector-ref botones-matriz i) j))
    (send boton-seleccionado set-label "X")  ;; Cambio: mostrar "X" solo cuando está seleccionado
    (send boton-seleccionado enable #f)
    
    ;; Verificar si la celda tiene bomba
    (define tiene-bomba (member (+ idx 1) lista-bombas))
    (define texto-bomba (if tiene-bomba "SÍ" "NO"))
    
    ;; Mostrar información detallada
    (define info-texto
      (string-append
       "=== CELDA SELECCIONADA ===\n"
       "Coordenadas: (" (number->string i-real) ", " (number->string j-real) ")\n"
       "Índice: " (number->string (+ idx 1)) "\n"  ;; Cambio: +1 para índice que empieza desde 1
       "Tiene bomba: " texto-bomba "\n"
       "Número de vecinos: " (number->string (length vecinos)) "\n"
       "Vecinos: " (string-join (map (λ (v) (number->string (+ v 1))) (sort vecinos <)) ", ")))  ;; Cambio: +1 para vecinos
    
    (send info-label set-label info-texto)))

(define (limpiar-seleccion)
  (when boton-seleccionado
    (send boton-seleccionado set-label " ")  ;; Cambio: dejar vacío al limpiar
    (send boton-seleccionado enable #t))
  (set! celda-seleccionada #f)
  (set! boton-seleccionado #f)
  (when info-label
    (send info-label set-label "Selecciona una celda para ver su información")))

(define (mostrar-todos-datos filas cols)
  (when mi-grafo
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
    
    (send texto-datos insert (format "=== MATRIZ ~ax~a - DATOS COMPLETOS ===\n\n" filas cols))
    
    (for* ([i (in-range filas)]
           [j (in-range cols)])
      (define idx (obtener-indice mi-grafo i j))
      (define coord (obtener-coordenadas mi-grafo idx))
      (define vecinos (obtener-vecinos mi-grafo idx))
      (define i-real (+ i 1))
      (define j-real (+ j 1))
      (define tiene-bomba (member (+ idx 1) lista-bombas))
      (define texto-bomba (if tiene-bomba "BOMBA" "LIBRE"))
      
      (send texto-datos insert 
            (format "Celda [~a,~a]: Índice ~a - ~a\n" i-real j-real (+ idx 1) texto-bomba))
      (send texto-datos insert 
            (format "  Vecinos: ~a\n\n" (sort (map (λ (v) (+ v 1)) vecinos) <))))  ;; Cambio: +1 para vecinos
    
    (send ventana-datos show #t)))



(define (buscar-por-indice idx filas cols)
  (when (and mi-grafo (number? idx) (>= idx 1) (<= idx (* filas cols)))  ;; Cambio: empieza desde 1
    (define i (quotient (- idx 1) cols))  ;; Cambio: ajuste para índice que empieza desde 1
    (define j (remainder (- idx 1) cols))
    (mostrar-info-celda i j)
    (when info-label
      (send info-label set-label 
            (string-append (send info-label get-label)
                           "\n\n¡Búsqueda exitosa! Celda encontrada.")))
    #t)
  #f)

(define (non-empty-string? str)
  (not (string=? (string-trim str) "")))

;; ==================== FUNCIÓN PRINCIPAL ====================
(provide iniciar-gui-game)

(define (iniciar-gui-game filas cols dificultad)
  (printf "Iniciando juego con ~a filas, ~a columnas, dificultad: ~a\n" filas cols dificultad)
  
  ;; Reiniciar estado del juego
  (set! juego-activo #t)
  
  ;; Crear el grafo con las dimensiones seleccionadas
  (set! mi-grafo (crear-grafo-matriz filas cols))
  
  ;; Crear la interfaz gráfica
  (crear-interfaz-matriz filas cols)
  
  ;; Crear matriz con bombas y guardar la lista de bombas
  (set! lista-bombas (crear-lista-bombas filas cols dificultad))
  (crear-matriz-con-bombas filas cols dificultad)
  
  ;; Mostrar la ventana
  (send frame show #t)
  
  (printf "Juego listo! Matriz ~ax~a creada.\n" filas cols)
  (printf "Bombas colocadas: ~a\n" lista-bombas))