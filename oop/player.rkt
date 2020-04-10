#lang racket
(require 2htdp/universe 2htdp/image)

(provide player% W H F)

;these constants define the size of the canvas
(define W 400)
(define H 200)

;the height of the floor for players
(define F (- H 21))

(define player%
  (class object%
    (super-new)

    ;x position of player
    (init x0)
    (define x x0)
    (define/public (get-x) x)

    ;y position of player
    (define y F)
    (define/public (get-y) y)

    ;velocity of player
    (define Vx 0)    
    (define Vy 0)

    ;player model/collision
    (init colour)
    (define model (rectangle 20 40 "solid" colour))
    (define/public (get-model) model)
    
    (struct inputs (left right up)
      #:transparent
      #:mutable)

    ;mapping the controls
    (init move-left)
    (init move-right)
    (init jump)
    (define key-map (inputs move-left move-right jump))

    ;the object's internal representation of which buttons
    ;are pressed
    (define key-state (inputs #f #f #f))
    (define (horiz-socd)
      (match key-state
        [(inputs #t #f _) -1]
        [(inputs #f #t _) 1]
        [else 0]))

    ;the current state of the player
    (define state "stand")

    ;read key presses or releases and modify the key-state
    (define/public (set-key key val)
      (cond
        [(key=? key (inputs-left key-map))
         (set-inputs-left! key-state val)]
        [(key=? key (inputs-right key-map))
         (set-inputs-right! key-state val)]
        [(key=? key (inputs-up key-map))
         (set-inputs-up! key-state val)])
      this)

    ;move the player and switch states depending on the
    ;key-state
    (define/public (move)
      (case state
        [("stand")
         (set! Vx (* 2 (horiz-socd)))
         (case (inputs-up key-state)
           [(#t)            
            (set! Vy 6)
            (set! state "jump")]
           [else
            (set! x (+ x Vx))])]
        [("jump")
         (cond
           [(and (>= y F) (<= Vy 0))             
            (set! y F)
            (set! Vy 0)
            (set! state "stand")]             
           [else
            (set! x (+ x Vx))
            (set! y (- y Vy))
            (set! Vy (- Vy 1))])])
      this)))