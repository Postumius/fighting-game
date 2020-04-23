#lang racket

(require 2htdp/universe 2htdp/image lang/posn
         "../helper-macros.rkt"
         "../geometry.rkt"
         "../interaction.rkt"
         racket/promise
         racket/generic)

(provide W player-x player-y player-up player-facing
         make-player get-frame read-key intention move)

;these constants define the size of the canvas
(define W 600)

;standing model
(define (standing colour)
  (place-bottom-left
   (overlay (rotate -90 (triangle 20 "solid" "red"))
            (rectangle 40 80 "solid" colour))
   40 0
   (rectangle 120 120 "solid" "transparent")))

(define sword (bitmap "./resources/bitmaps/sword.png"))

;a kick animation
(define (kick colour)
  (map
   (λ (w)
     (place-bottom-left
      (overlay (rotate -90 (triangle 20 "solid" "red"))
            (rectangle 40 80 "solid" colour))
      40 0
      (place-bottom-left
       sword (+ 40 w) 10
       (rectangle 120 120 "solid" "transparent"))))
   (append
    (range 0 40 (/ 40 6))
    (build-list 4 (λ(n) 40))
    (range 40 0 (-(/ 40 15))))))


(define-generics character
  [coll character]
  [Vxmax character])

;TODO: make atks a hash, put anims in character-specific array
(struct atk (interrupt state key anim))
(struct dir-keys (up left right) #:transparent)

(struct player  
  (;fluid
   state
   facing
   x y Vx Vy
   atks
   up left right
   anim

   ;fixed   
   dir-keys   
   colour)
  
   #:methods gen:character
   [(define (coll p)
      (htbox (player-x p) (player-y p) 40 80))
    (define (Vxmax p) 2)])

;initialize a player
(define/contract
  (make-player x mk-key up-key left-key right-key colour)
  (-> real?
      key-event? key-event? key-event? key-event?
      image-color? player?)
  (player
   'stand
   #t
   x 1 0 0
   (list
    (atk (cons #f #f) #f mk-key (kick colour)))   
   #f #f #f
   (list (standing colour))
   (dir-keys up-key left-key right-key)
   colour))

(define p1
  (make-player
   0 "s" "w" "a" "d" "aquamarine"))
(define p2
  (struct-copy
   player 
   (make-player
    40 "k" "i" "j" "l" "medium gray")
   [facing #f]))

(define/contract (get-frame p)
  (-> player? image?)
  (define frame (car (player-anim p)))
  (if (player-facing p)
      frame
      (flip-horizontal frame)))


;read one keyboard input
(define/contract (read-key p key val)
  (-> player? key-event? boolean? player?)
  
  (define (push-shift new interrupt)
    (cons new (car interrupt)))
  (define (read-atks atks)
    (match atks
      ['() '()]
      [(cons a as)
       (if (key=? key (atk-key a))
           (cons 
            (struct-copy*
             atk a
             [interrupt (push-shift val (atk-interrupt a))]
             [state
              (match (atk-interrupt a)
                [(cons #t #f) #t]
                [_ (atk-state a)])])
            as)
           (cons a (read-atks as)))]))           
  
  (match (player-dir-keys p)
    [(dir-keys up-key left-key right-key)
     (cond
       [(key=? key up-key)
        (struct-copy player p [up val])]
       [(key=? key left-key)
        (struct-copy player p [left val])]
       [(key=? key right-key)
        (struct-copy player p [right val])]
       [else
        (struct-copy
         player p
         [atks (read-atks (player-atks p))])])]))


;change states depending on input
(define/contract (intention p)
  (-> player? player?) 
  (define (horiz-socd)
    (multi-match
       ((player-left p) (player-right p))
       [(#t #f) -1]
       [(#f #t) 1]
       [(_ _) 0]))
  (define (vert-socd)
    (multi-match
     ((player-up p) 'todo-put-down-here)
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
         [Vx 0]
         [anim (atk-anim attack)]         
         [atks (map (λ (attack)
                      (struct-copy atk attack [state #f]))
                    (player-atks p))])]
       [(= (vert-socd) 1)
        (struct-copy
         player p
         [Vx (* 2 (horiz-socd))]
         [Vy 10]
         [state 'jump])]
       [else
        (struct-copy
         player p      
         [Vx (* 2 (horiz-socd))])])]
    ['jump
     (if (and (<= (player-y p) 1) (<= (player-Vy p) 0))
         (struct-copy
          player p
          [y 1]
          [Vy 0]
          [state 'stand])
         p)]

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


;return the x position after collision
(define (collision p other)
  (let* ([leading-edge
          (λ (p)
            (if (player-facing p)
                (+ (player-x p) (htbox-w (coll p)))
                (player-x p)))]
         [leading-edge->x
          (λ (edge)
            (if (player-facing p)
                (- edge (htbox-w (coll p)))
                edge))]       
         [towards?
          (λ (p)
            (define Vx (player-Vx p))
            (if (player-facing p)
                (positive? Vx)
                (negative? Vx)))]
         [away?
          (λ (p)
            (and (not (towards? p))
                 (not (zero? (player-Vx p)))))]
         [abs-min
          (λ (v1 v2)
            (if (< (abs v1) (abs v2))
                v1
                v2))]        
         [x (player-x p)]
         [Vx (player-Vx p)]
         [u (player-x other)]
         [Vu (player-Vx other)]
         [dest (+ x Vx)]
         [push-against
          (λ ()
            (cond              
              [(and (towards? p) (towards? other))
               x]
              [(and (towards? p) (zero? Vu))
               (+ x (/ Vx 2))]
              [(and (zero? Vx) (towards? other))
               (+ x (/ Vu 2))]
              [(and (towards? p) (away? other))
               (+ x (min Vx Vu))]
              [(or (not (towards? p)) (not (towards? other)))
               dest]))])
    (cond
      [(= (leading-edge p) (leading-edge other))
       (push-against)]
      [(boxes-overlap?
        (struct-copy htbox (coll p) [x dest])
        (struct-copy htbox (coll other) [x (+ u Vu)]))                     
       (leading-edge->x
        (locate-collision
         (leading-edge p) Vx
         (leading-edge other) Vu))]
      [else dest])))
 


;move the player, with input from the other player
(define/contract (move p other)
  (-> player? player? player?)
  (define (update-facing)
    (case (player-facing p)
      [(#t) (if (<= (player-x p) (player-x other))
                #t #f)]
      [(#f) (if (<= (player-x other) (player-x p))
                #f #t)]))
  
  (case (player-state p)
    ['stand
     (struct-copy
      player p
      [facing (update-facing)]
      [x (collision p other)])]
    ['jump
     (struct-copy
         player p
         [x (+ (player-x p) (player-Vx p))]
         [y (+ (player-y p) (player-Vy p))]
         [Vy (- (player-Vy p) 1)])]
    ['animate
     (struct-copy
      player p
      [x (collision p other)])]))

