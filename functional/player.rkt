#lang racket

(require 2htdp/universe 2htdp/image lang/posn
         "../helper-macros.rkt"
         "../geometry.rkt"
         racket/promise)

(provide W H player-x player-y player-up make-player get-frame
         read-key move)

;these constants define the size of the canvas
(define W 600)
(define H 300)


;standing model
(define (standing colour)
  (place-bottom-left
   (overlay (rotate -90 (triangle 20 "solid" "red"))
            (rectangle 40 80 "solid" colour))
   40 0
   (rectangle 120 120 "solid" "transparent")))

;a kick animation
(define (kick colour)
  (map
   (λ (leg-colour w)
     (beside
      (rectangle w 80 "solid" (color 255 255 255 0))
      (standing colour)
      (rectangle w 20 "solid" leg-colour)))
   (append
    (build-list 6 (λ(n) colour))
    (build-list 4 (λ(n) "red"))
    (build-list 15 (λ(n) colour)))
   (append
    (range 0 40 (/ 40 6))
    (build-list 4 (λ(n) 40))
    (range 40 0 (-(/ 40 15))))))

(struct atk (interrupt state key anim))
(struct player
  (state
   facing
   x y Vx Vy
   atks
   up left right
   colour
   anim))

(define
  (make-player x mk-key up-key left-key right-key colour)
  (player
   'stand
   #t
   x 1 0 0
   (list
    (atk (cons #f #f) #f mk-key (kick colour)))
   (cons up-key #f)
   (cons left-key #f)
   (cons right-key #f)
   colour
   (list (standing colour))))

(define/contract (get-frame p)
  (-> player? image?)
  (define frame (car (player-anim p)))
  (if (player-facing p)
      frame
      (flip-horizontal frame)))


;read one keyboard input
(define/contract (read-key p key val)
  (-> player? key-event? boolean? player?)
  (define (read-atks)
    (define (push-shift new interrupt)
      (cons new (car interrupt)))
    (map
     (λ (attack)
       (if (key=? key (atk-key attack))
           (struct-copy*
            atk attack
            [interrupt (push-shift val (atk-interrupt attack))]
            [state
             (match (atk-interrupt attack)
               [(cons #t #f) #t]
               [_ (atk-state attack)])])
           attack))
     (player-atks p)))
  
  (define (read-dir dir)
    (if (key=? key (car dir))
        (cons (car dir) val)
        dir))

  (struct-copy
   player p
   [atks (read-atks)]
   [up (read-dir (player-up p))]
   [left (read-dir (player-left p))]
   [right (read-dir (player-right p))]))


;move the player and change states
(define/contract (move p other-x)
  (-> player? real? player?)
  
  (define (update-facing)
    (define x (player-x p))
    (case (player-facing p)
      [(#t) (if (<= x other-x)
                #t #f)]
      [(#f) (if (<= other-x x)
                #f #t)]))
  (define horiz-socd
    (delay
      (multi-match
       ([cdr (player-left p)] [cdr (player-right p)])
       [(#t #f) -1]
       [(#f #t) 1]
       [(_ _) 0])))
  (define (vert-socd)
    (multi-match
     ([cdr (player-up p)] 'put-down-here)
     [(#t _) 1]
     [(_ _) 0]))
  (define attack (findf atk-state (player-atks p)))
  
  (case (player-state p)
    ['stand        
     (cond
       [attack
        (struct-copy
         player p
         [state 'animate]
         [anim (atk-anim attack)]
         [atks (map (λ (attack)
                      (struct-copy atk attack [state #f]))
                    (player-atks p))])]
       [(= (vert-socd) 1)
        (struct-copy
         player p
         [Vx (* 2 (force horiz-socd))]
         [Vy 10]
         [state 'jump])]
       [else
        (struct-copy*
         player p
         [facing (update-facing)]
         [Vx (* 2 (force horiz-socd))]
         [x (+ (player-x p) (player-Vx p))])])]

    ['jump
     (cond
       [(and (<= (player-y p) 1) (<= (player-Vy p) 0))
        (struct-copy
         player p
         [y 1]
         [Vy 0]
         [state 'stand])]
       [else
        (struct-copy
         player p
         [x (+ (player-x p) (player-Vx p))]
         [y (+ (player-y p) (player-Vy p))]
         [Vy (- (player-Vy p) 1)])])]

    ['animate
     (match (player-anim p)
       [(cons _ '())
        (struct-copy
         player p
         [anim (list (standing (player-colour p)))]
         [state 'stand])]
       [(cons _ next-frame-anim)
        (struct-copy
         player p
         [anim next-frame-anim])])]))
     

