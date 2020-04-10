#lang racket
(require 2htdp/universe 2htdp/image)

(define W 400)
(define H 200)
(define F (- H 21))

(define player%
  (class object%
    (super-new)
    
    (init x0)
    (define x x0)
    (define/public (get-x) x)
    
    (define y F)
    (define/public (get-y) y)
    
    (define Vx 0)    
    (define Vy 0)

    (init colour)
    (define model (rectangle 20 40 "solid" colour))
    (define/public (get-model) model)

    (struct inputs (left up right)
      #:transparent
      #:mutable)
    (define keys (inputs #f #f #f))
    (define (horiz-socd)
      (match keys
        [(inputs #t _ #f) -1]
        [(inputs #f _ #t) 1]
        [else 0]))

    (define state "stand")

    (define/public (set-key key val)
      (cond
        [(key=? key "left") (set-inputs-left! keys val)]
        [(key=? key "right") (set-inputs-right! keys val)]
        [(key=? key "up") (set-inputs-up! keys val)])
      this)

    (define/public (move)
      (case state
        [("stand")
         (case (inputs-up keys)
           [(#t)
            (set! Vx (* 2 (horiz-socd)))
            (set! Vy 6)
            (set! state "jump")]
           [else
            (set! Vx (* 2 (horiz-socd)))
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
      

(define (draw-players p1)
  (place-image
   (send p1 get-model)
   (send p1 get-x)
   (send p1 get-y)
   (empty-scene W H)))

(big-bang
    (new player%
         [x0 (/ W 2)]
         [colour "aquamarine"])
  (on-tick (λ (p) (send p move)))
  (on-key (λ (p key) (send p set-key key #t)))
  (on-release (λ (p key) (send p set-key key #f)))
  (to-draw draw-players))