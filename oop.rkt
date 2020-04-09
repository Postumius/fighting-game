#lang racket
(require 2htdp/universe 2htdp/image)

(define W 400)
(define H 200)
(define F (- H 20))

(define player%
  (class object%
    (init x0)
    (define x x0)
    (define/public (get-x) x)
    
    (init y0)
    (define y y0)
    (define/public (get-y) y)
    
    (init Vx0)
    (define Vx Vx0)
    (init Vy0)
    (define Vy Vy0)
    (define model (rectangle 20 40 "solid" "maroon"))
    (define/public (get-model) model)

    (define state "stand")

    (super-new)

    
    (define/public (press key)
      (case state
        [("stand")
         (cond
           [(key=? key "left") (set! Vx -2)]
           [(key=? key "right") (set! Vx 2)]
           [(key=? key "up")
            (begin
              (set! Vy 6)
              (set! state "jump"))])]
        [("jump") null])
      this)

     (define/public (release key)
      (case state
        [("stand")
         (cond
           [(key=? key "left") (set! Vx 0)]
           [(key=? key "right") (set! Vx 0)])]
        [("jump") null])
      this)

    (define/public (move)      
      (set! x (+ x Vx))
      (case state
        [("stand")
         (if (> Vy 0)
             (set! state "jump")
             null)]
        [("jump")
         (if (and (>= y F) (<= Vy 0))
             (begin
               (set! state "stand")
               (set! y F)
               (set! Vy 0)
               (set! Vx 0))
             (begin
               (set! y (- y Vy))
               (set! Vy (sub1 Vy))))])         
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
         [y0 F]
         [Vx0 0]
         [Vy0 0])
  (on-tick (λ (p) (send p move)))
  (on-key (λ (p key) (send p press key)))
  (on-release (λ (p key) (send p release key)))
  (to-draw draw-players))