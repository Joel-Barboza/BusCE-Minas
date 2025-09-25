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
    (vector-set! nodos idx (list (+ i 1) (+ j 1) 'libre)))
  
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
(define game-canvas #f)
(define info-label #f)
(define celda-seleccionada #f)
(define lista-bombas '())
(define juego-activo #t)
(define cell-size 40) ; tamaño de cada celda en píxeles
(define estado-celdas (make-hash)) ; almacena el estado de cada celda (oculta, revelada, bandera)

;; ==================== FUNCIONES PARA VERIFICAR VICTORIA ====================
(define (verificar-victoria)
  (when (and juego-activo mi-grafo)
    (define filas (grafo-matriz-filas mi-grafo))
    (define cols (grafo-matriz-columnas mi-grafo))
    (define total-celdas (* filas cols))
    (define celdas-reveladas 0)
    (define celdas-sin-bomba (- total-celdas (length lista-bombas)))
    
    ;; Contar celdas reveladas
    (for ([i filas])
      (for ([j cols])
        (define key (string->symbol (format "~a-~a" i j)))
        (define estado (hash-ref estado-celdas key 'oculta))
        (when (eq? estado 'revelada)
          (set! celdas-reveladas (+ celdas-reveladas 1)))))
    
    ;; Verificar si todas las celdas sin bomba están reveladas
    (when (>= celdas-reveladas celdas-sin-bomba)
      (ganar-juego))))

(define (ganar-juego)
  (set! juego-activo #f)
  (send info-label set-label "¡FELICIDADES! Has ganado el juego.")
  (message-box "¡Victoria!" 
               "¡FELICIDADES! Has revelado todas las celdas seguras.\n¡Has ganado el juego!" 
               frame 
               '(ok)))

;; ==================== CANVAS PERSONALIZADO ====================
(define game-canvas%
  (class canvas%
    (super-new)
    
    (define/override (on-event evt)
      (when juego-activo
        (define x (send evt get-x))
        (define y (send evt get-y))
        (define col (quotient x cell-size))
        (define row (quotient y cell-size))
        (define filas (grafo-matriz-filas mi-grafo))
        (define cols (grafo-matriz-columnas mi-grafo))
        
        ;; Verificar que el click esté dentro de los límites
        (when (and (>= row 0) (< row filas) (>= col 0) (< col cols))
          (cond
            [(send evt button-down? 'left)
             (manejar-click-celda row col)]
            [(send evt button-down? 'right)
             (manejar-click-derecho row col)]))))
    
    (define/override (on-paint)
      (define dc (send this get-dc))
      (define filas (grafo-matriz-filas mi-grafo))
      (define cols (grafo-matriz-columnas mi-grafo))
      
      ;; Pintar todas las celdas
      (for ([i filas])
        (for ([j cols])
          (dibujar-celda dc i j))))))

;; ==================== FUNCIONES DE DIBUJO ====================
(define (dibujar-celda dc i j)
  (define x (* j cell-size))
  (define y (* i cell-size))
  (define idx (obtener-indice mi-grafo i j))
  (define key (string->symbol (format "~a-~a" i j)))
  (define estado (hash-ref estado-celdas key 'oculta))
  
  (case estado
    ['oculta
     (send dc set-brush "lightblue" 'solid)
     (send dc set-pen "black" 1 'solid)
     (send dc draw-rectangle x y cell-size cell-size)]
    
    ['revelada
     (send dc set-brush "white" 'solid)
     (send dc set-pen "black" 1 'solid)
     (send dc draw-rectangle x y cell-size cell-size)
     
     ;; Mostrar número de bombas vecinas si es mayor a 0
     (define vecinos (obtener-vecinos mi-grafo idx))
     (define num-bombas (bombas-vecinas idx vecinos lista-bombas))
     (when (> num-bombas 0)
       (send dc set-text-foreground (get-color-numero num-bombas))
       (send dc set-font (make-font #:size 12 #:weight 'bold))
       (send dc draw-text (number->string num-bombas) 
             (+ x (/ cell-size 3)) (+ y (/ cell-size 4))))]
    
    ['bandera
     (send dc set-brush "orange" 'solid)
     (send dc set-pen "black" 1 'solid)
     (send dc draw-rectangle x y cell-size cell-size)
     (send dc set-text-foreground "red")
     (send dc set-font (make-font #:size 10 #:weight 'bold))
     (send dc draw-text "🚩" (+ x 8) (+ y 8))])
  
  ;; Dibujar borde
  (send dc set-pen "black" 2 'solid)
  (send dc set-brush "white" 'transparent)
  (send dc draw-rectangle x y cell-size cell-size))

(define (get-color-numero num)
  (case num
    [(1) "blue"]
    [(2) "green"]
    [(3) "red"]
    [(4) "purple"]
    [(5) "maroon"]
    [(6) "turquoise"]
    [(7) "black"]
    [(8) "gray"]
    [else "black"]))

;; ==================== FUNCIONES DE LA INTERFAZ ====================
(define (crear-interfaz-matriz filas cols)
  ;; Destruir frame anterior si existe
  (when frame
    (send frame show #f))
  
  (set! frame (new frame%
                   [label (format "Buscaminas ~ax~a" filas cols)]
                   [width (min 800 (* cols cell-size))]
                   [height (min 650 (+ (* filas cell-size) 100))])) ; espacio extra para controles
  
  ;; Panel principal
  (define panel-principal (new vertical-panel%
                               [parent frame]
                               [alignment '(center center)]
                               [spacing 10]))
  
  ;; Panel para la matriz (canvas)
  (define panel-matriz-container (new group-box-panel%
                                      [parent panel-principal]
                                      [label (format "Matriz ~ax~a" filas cols)]
                                      [min-width (* cols cell-size)]
                                      [min-height (* filas cell-size)]))
  
  ;; Crear el canvas personalizado
  (set! game-canvas (new game-canvas%
                         [parent panel-matriz-container]
                         [min-width (* cols cell-size)]
                         [min-height (* filas cell-size)]))
  
  ;; Inicializar estados de las celdas
  (hash-clear! estado-celdas)
  (for ([i filas])
    (for ([j cols])
      (hash-set! estado-celdas (string->symbol (format "~a-~a" i j)) 'oculta)))
  
  ;; Etiqueta de información
  (set! info-label (new message%
                        [parent panel-principal]
                        [label "Haz click izquierdo para revelar una celda, click derecho para poner/quitar bandera"]
                        [auto-resize #t]
                        [min-width 600]))
  
  ;; Panel de control
  (define panel-control (new horizontal-panel%
                             [parent panel-principal]
                             [alignment '(center center)]
                             [spacing 10]))
  
  (new button% [parent panel-control]
       [label "Volver al inicio"]
       [callback
        (λ (b e)
          (send frame show #f)        ; ocultar la ventana del juego
          (send start-frame show #t))]) ; mostrar la pantalla de inicio

  (new button% [parent panel-control]
       [label "Mostrar Bombas"]
       [callback (λ (b e) (mostrar-todas-bombas))]))

(define (manejar-click-celda i j)
  (when juego-activo
    (define idx (obtener-indice mi-grafo i j))
    (define indice-real (+ idx 1))
    (define tiene-bomba (member indice-real lista-bombas))
    (define key (string->symbol (format "~a-~a" i j)))
    (define estado (hash-ref estado-celdas key))
    
    (when (eq? estado 'oculta)
      (if tiene-bomba
          (perder-juego i j)
          (begin
            (hash-set! estado-celdas key 'revelada)
            (mostrar-info-celda i j)
            (send game-canvas refresh)
            (verificar-victoria)))))) ; Verificar victoria después de revelar celda

(define (manejar-click-derecho i j)
  (when juego-activo
    (define key (string->symbol (format "~a-~a" i j)))
    (define estado (hash-ref estado-celdas key))
    
    (case estado
      ['oculta 
       (hash-set! estado-celdas key 'bandera)
       (send game-canvas refresh)]
      ['bandera 
       (hash-set! estado-celdas key 'oculta)
       (send game-canvas refresh)])))

(define (perder-juego i j)
  (set! juego-activo #f)
  (define key (string->symbol (format "~a-~a" i j)))
  (hash-set! estado-celdas key 'revelada)
  
  ;; Mostrar todas las bombas
  (for ([bomba lista-bombas])
    (define idx (- bomba 1))
    (define cols (grafo-matriz-columnas mi-grafo))
    (define i-bomba (quotient idx cols))
    (define j-bomba (remainder idx cols))
    (define key-bomba (string->symbol (format "~a-~a" i-bomba j-bomba)))
    (hash-set! estado-celdas key-bomba 'revelada))
  
  (send game-canvas refresh)
  (send info-label set-label "¡PERDISTE! Has pisado una bomba. El juego ha terminado.")
  (message-box "Game Over" 
               "¡PERDISTE! Has pisado una bomba.\nEl juego ha terminado." 
               frame 
               '(ok stop)))

(define (mostrar-todas-bombas)
  (for ([bomba lista-bombas])
    (define idx (- bomba 1))
    (define cols (grafo-matriz-columnas mi-grafo))
    (define i-bomba (quotient idx cols))
    (define j-bomba (remainder idx cols))
    (define key-bomba (string->symbol (format "~a-~a" i-bomba j-bomba)))
    (hash-set! estado-celdas key-bomba 'revelada))
  (send game-canvas refresh))

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
    (define i-real (+ i 1))
    (define j-real (+ j 1))
    
    (define tiene-bomba (member (+ idx 1) lista-bombas))
    (define texto-bomba (if tiene-bomba "SÍ" "NO"))
    
    (define info-texto
      (string-append
       "=== CELDA SELECCIONADA ===\n"
       "Coordenadas: (" (number->string i-real) ", " (number->string j-real) ")\n"
       "Índice: " (number->string (+ idx 1)) "\n"
       "Tiene bomba: " texto-bomba "\n"
       "Número de vecinos: " (number->string (length vecinos)) "\n"
       "Bombas vecinas: " (number->string (bombas-vecinas idx vecinos lista-bombas)) "\n"
       "Vecinos: " (string-join (map (λ (v) (number->string (+ v 1))) (sort vecinos <)) ", ")))
    
    (send info-label set-label info-texto)))

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
  
  ;; Mostrar la ventana
  (send frame show #t)
  
  (printf "Juego listo! Matriz ~ax~a creada.\n" filas cols)
  (printf "Bombas colocadas: ~a\n" lista-bombas))