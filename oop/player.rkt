#lang racket
(require 2htdp/universe 2htdp/image)

(provide player% W H F)

;these constants define the size of the canvas
(define W 600)
(define H 300)

;the height of the floor for players
(define F (- H 41))

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

    ;facing of player
    (define facing-right #t)

    ;player model/collision
    (init colour)
    (define model
      (overlay (rotate -90 (triangle 20 "solid" "red"))
               (rectangle 40 80 "solid" colour)))
    (define/public (get-model)
      (if facing-right
          (first frame-anim)
          (flip-horizontal (first frame-anim))))

    ;currently displayed frame
    (define frame-anim (list model))
    
    ;a kick animation
    (define kick
      (map
       (λ (leg-colour w)
         (beside
          (rectangle w 80 "solid" (color 255 255 255 0))
          model
          (rectangle w 20 "solid" leg-colour)))
       (append
        (build-list 6 (λ(n) colour))
        (build-list 4 (λ(n) "red"))
        (build-list 15 (λ(n) colour)))
       (append
        (range 0 40 (/ 40 6))
        (build-list 4 (λ(n) 40))
        (range 40 0 (-(/ 40 15))))))
    
    (struct inputs (left right up mk)
      #:transparent
      #:mutable)

    ;mapping the controls
    (init move-left)
    (init move-right)
    (init jump)
    (init med-kick)
    (define key-map (inputs move-left move-right
                            jump med-kick))

    ;the object's internal representation of which buttons
    ;are pressed
    (define key-state (inputs #f #f #f #f))
    (define (horiz-socd)
      (match key-state
        [(inputs #t #f _ _) -1]
        [(inputs #f #t _ _) 1]
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
         (set-inputs-up! key-state val)]
        [(key=? key (inputs-mk key-map))
         (set-inputs-mk! key-state val)])
      this)

    (define (update-facing other-x)
      (if (<= x other-x)
          (set! facing-right #t)
          (set! facing-right #f)))

    ;move the player and switch states depending on the
    ;key-state
    (define/public (move)
      (case state
        [("stand")
         (update-facing (/ W 2))
         (set! Vx (* 2 (horiz-socd)))
         (match key-state
           [(inputs _ _ _ #t)
            (set! state "animating")
            (set! frame-anim kick)]
           [(inputs _ _ #t _)
            (set! Vy 6)
            (set! state "jump")]           
           [_
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
            (set! Vy (- Vy 1))])]
        [("animating")
         (set! frame-anim (rest frame-anim))
         (cond
           [(null? frame-anim)
            (set! state "stand")
            (set! frame-anim (list model))])])
      this)))