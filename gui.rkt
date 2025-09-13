#lang racket/gui
(require "gui_game.rkt") ;; Importar la función iniciar-gui-game

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


;; Botón de iniciar
(new button% [parent main-panel]
             [label "Iniciar"]
             [callback
              (lambda (button event)
                (define filas (string->number (send filas-choice get-string-selection)))
                (define cols (string->number (send cols-choice get-string-selection)))
                (define dificultad (send dificultad-choice get-string-selection))
                (send start-frame show #f) ;; oculta pantalla de inicio
                (iniciar-gui-game filas cols dificultad))])

(send start-frame show #t)
