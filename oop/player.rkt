#lang racket
(require 2htdp/universe 2htdp/image lang/posn)
(require "../helper-macros.rkt")

(provide player% W H F)

`
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
    
    

    ;mapping the controls
    (init left-button)       
    (init right-button)     
    (init up-button)
    (init med-kick-button)
    (struct keys (left right up mk) #:transparent)
    (define key-map (keys left-button right-button
                          up-button med-kick-button))
    
    ;the object's internal representation of button presses
    (define left #f) 
    (define right #f)   
    (define up #f)    
    (define mk #f)    
    (define mk-interrupt (cons #f #f))     

    ;directional keys register holding; attack keys don't
    (define/public (set-key key val)
      (define (push-shift new interrupt)
        (cons new (car interrupt)))
      (cond
        [(key=? key (keys-left key-map))
         (set! left val)]
        [(key=? key (keys-right key-map))
         (set! right val)]
        [(key=? key (keys-up key-map))
         (set! up val)]
        [(key=? key (keys-mk key-map))
         (set! mk-interrupt (push-shift val mk-interrupt))
         (match mk-interrupt
           [(cons #t #f) (set! mk #t)]
           [_ void])]
        [else void])
      this)
    
    (define (horiz-socd)
      (multi-match
       (left right)
        [(#t #f) -1]
        [(#f #t) 1]
        [(_ _) 0]))

    ;the current state of the player
    (define state "stand")
    
    (define (update-facing other-x)
      (if (<= x other-x)
          (set! facing-right #t)
          (set! facing-right #f)))

    ;move the player and switch states depending on the
    ;key-state
    (define/public (move other-x)
      (case state
        [("stand")
         (update-facing other-x)
         (set! Vx (* 2 (horiz-socd)))
         (cond
           [mk
            (set! state "animating")
            (set! frame-anim kick)]
           [up
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
            (set! Vy (- Vy 1))])]
        [("animating")
         (set! frame-anim (rest frame-anim))
         (cond
           [(null? frame-anim)
            (set! state "stand")
            (set! frame-anim (list model))])])
      (set! mk #f)
      this)))