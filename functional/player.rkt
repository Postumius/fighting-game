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
  [Vxmax character]
  [jump-speed character]
  [gravity character])

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
  ([define (coll p)
     (htbox (player-x p) (player-y p) 40 80)]
   [define (Vxmax p) 2]
   [define (jump-speed p) 8]
   [define (gravity p) 1/3]))

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
         [Vy (jump-speed p)]
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


(define (update-facing p other)
    (case (player-facing p)
      [(#t) (if (<= (player-x p) (player-x other))
                #t #f)]
      [(#f) (if (<= (player-x other) (player-x p))
                #f #t)]))

;return the x position after collision
(define (collision pl other-pl)
  [define p
    (struct-copy
     player pl
     [facing (update-facing pl other-pl)])]
  [define other
    (struct-copy
     player other-pl
     [facing (update-facing other-pl pl)])]
  [define (leading-edge facing collbox)
    (if facing
        (+ (htbox-x collbox) (htbox-w collbox))
        (htbox-x collbox))]
  [define (leading-edge->x edge facing collbox)
    (if facing
        (- edge (htbox-w collbox))
        edge)]
  [define (moving-in? p)   
    (define Vx (player-Vx p))
    (if (player-facing p)
        (positive? Vx)
        (negative? Vx))]
  [define (away? p)   
    (and (not (moving-in? p))
         (not (zero? (player-Vx p))))]
  [define (abs-min v1 v2)
    (if (< (abs v1) (abs v2))
        v1
        v2)]
  [define front (player-facing p)]
  [define x (player-x p)]
  [define Vx (player-Vx p)]
  [define u (player-x other)]
  [define Vu (player-Vx other)]
  [define dest (+ x Vx)]  
  [define (push-against)   
    (cond              
      [(and (moving-in? p) (moving-in? other))
       (+ x Vx Vu)]
      [(and (moving-in? p) (zero? Vu))
       (+ x (/ Vx 2))]
      [(and (zero? Vx) (moving-in? other))
       (+ x (/ Vu 2))]
      [(and (moving-in? p) (away? other))
       (+ x (min Vx Vu))]
      [(or (not (moving-in? p)) (not (moving-in? other)))
       dest])]              
  (cond
    [(touch-horiz? (coll p) (coll other))
     (push-against)]
    [(boxes-overlap?
      (struct-copy htbox (coll p) [x dest])
      (struct-copy htbox (coll other) [x (+ u Vu)]))
     (leading-edge->x      
      (locate-center
       (leading-edge front (coll p))
       (leading-edge (not front) (coll other)))
      front
      (coll p))]
    [else dest]))
 


;move the player, with input from the other player
(define/contract (move p other)
  (-> player? player? player?)
  
  
  (case (player-state p)
    ['stand
     (struct-copy
      player p
      [facing (update-facing p other)]
      [x (collision p other)])]
    ['jump
     (struct-copy
         player p
         [x (collision p other)]
         [y (round (+ (player-y p) (player-Vy p)))]
         [Vy (- (player-Vy p) (gravity p))])]
    ['animate
     (struct-copy
      player p
      [x (collision p other)])]))

