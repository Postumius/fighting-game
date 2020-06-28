#lang racket

(require 2htdp/universe 2htdp/image 
         "../helper-macros.rkt"
         "../geometry.rkt"
         racket/promise
         "../animation-test.rkt" ;misleading name,
                                 ;contains character animations
         "../helper.rkt"
         data/collection
         "../struct+/struct+.rkt"
         "../struct+/keywords.rkt"
         racket/flonum)

(provide W make-player get-frame read-key intent act)

(define W 600.)
(define BLOCK-DISTANCE 100.)

(define sword (bitmap "./resources/bitmaps/sword.png"))

(struct+ Player  
  (;fluid
   state
   facing
   x y Vx Vy
   atks
   up left right
   anim
   fixed
   char))
(struct+ Atk (history state key anim))
(struct+ Fixed (dir-keys colour))
(struct+ Dir-keys (up-key left-key right-key))
(struct+ Char (coll-r coll-h Vxmax jump-speed gravity))

;TODO: make atks a hash, put anims in character-specific array
;(struct atk (interrupt state key anim))

;initialize a player
(define/contract/keywords
  (make-player x mk-key up-key left-key right-key colour)
  (-> real?
      key-event? key-event? key-event? key-event?
      image-color?
      Player?)
  (Player/keywords
   #:state 'stand
   #:facing #t
   #:x (exact->inexact x) #:y 1.0 #:Vx 0.0 #:Vy 0.0
   #:atks (list
          (Atk/keywords
                #:history (cons #f #f)
                #:state #f
                #:key mk-key
                #:anim (shine colour)))   
   #:up #f #:left #f #:right #f
   #:anim (list (standing-anim colour))
   #:fixed (Fixed/keywords
            #:dir-keys (Dir-keys/keywords
                        #:up-key up-key
                        #:left-key left-key
                        #:right-key right-key)
            #:colour colour)
   #:char (Char/keywords
           #:coll-r 20.0
           #:coll-h 80.0
           #:Vxmax 2.0
           #:jump-speed 8.0
           #:gravity (fl/ 1.0 3.0))))

(define p1
  (make-player
   #:x 0 #:mk-key "s" #:up-key "w"
   #:left-key "a" #:right-key "d"
   #:colour "aquamarine"))
(define p2
  (make-player
   #:x 100 #:mk-key "k" #:up-key "i"
   #:left-key "j" #:right-key "l"
   #:colour "medium gray"))

(define/contract (get-frame p)
  (-> Player? image?)
  (define frame (draw (p 'anim)))
  (if (p 'facing)
      frame
      (flip-horizontal frame)))


;read one keyboard input
(define/contract (read-key p key val)
  (-> Player? key-event? boolean? Player?)  
  (define (read-atks atks)
    (match atks
      [(sequence) '()]
      [(sequence a as ...)
       (if (key=? key (a 'key))
           (let ([shifted (cons val (car (a 'history)))])
             (conj
              as
              (struct-match-copy
               Atk a
               [history shifted]
               [state (match shifted
                        [(cons #t #f) #t]
                        [_ (a 'state)])])))
           (conj (read-atks as) a))]))
  
  (match (deep-ref p '(fixed dir-keys))
    [(Dir-keys up-key left-key right-key)
     (cond
       [(key=? key up-key)
        (struct-set p 'up val)]
       [(key=? key left-key)
        (struct-set p 'left val)]
       [(key=? key right-key)
        (struct-set p 'right val)]
       [else
        (struct-set p 'atks (read-atks (p 'atks)))])]))

(define (horiz-socd p)
    (match*
       ((p 'left) (p 'right))
       [(#t #f) -1.0]
       [(#f #t) 1.0]
       [(_ _) 0.0]))

  (define (vert-socd p)
    (match*
     ((p 'up) 'todo-put-down-here)
     [(#t _) 1.0]
     [(_ _) 0.0]))

;change states depending on input
(define/contract (intent p)
  (-> Player? Player?)   
  [define attack (finds (λ(a) (a 'state)) (p 'atks))]
  [define (new-Vx p)
       (fl* (deep-ref p '(char Vxmax)) (horiz-socd p))]
  
  (case (p 'state)
    [(stand block)
     (cond
       [attack
        (struct-match-copy
         Player p
         [state 'attack]
         [Vx 0.]
         [anim (attack 'anim)]
         [atks (map (λ(a) (struct-set a 'state #f))
                    atks)])]
       [(fl= (vert-socd p) 1.)
        (struct-copy
         Player p
         [Vx (new-Vx p)]
         [Vy (deep-ref p '(char jump-speed))]
         [state 'jump])]
       [else
        (struct-copy
         Player p
         [Vx (new-Vx p)])])]
    ['jump
     (if (and ((p 'y) . fl<= . 1.) ((p 'Vy) . fl<= . 0.))
         (struct-copy
          Player p
          [y 1.]
          [Vy 0.]
          [state 'stand])
         p)]

    [(animate attack)
     (match (p 'anim)
       [(sequence _)
        (struct-copy
         Player p
         [anim (list (standing-anim
                      (deep-ref p '(fixed colour))))]
         [state (if ((p 'y) . fl> . 1.) 'jump 'stand)])]
       [(sequence _ next-frame-anim ...)
        (struct-copy
         Player p
         [anim next-frame-anim]
         [Vx (let ([speed ((first next-frame-anim) 'speed)])
               (if (p 'facing) speed (fl* -1. speed)))])])]))

(define/contract (towards-facing p other)
  (-> Player? Player? boolean?)
    (case (p 'facing)
      [(#t) ((p 'x) . fl<= . (other 'x))]
      [(#f) (not ((other 'x) . fl<= . (p 'x)))]))

;get the x position after collision
(define (collision-move p other)
  [define (leading-edge facing collbox)
    ((if facing fl+ fl-) (collbox 'x) (collbox 'r))]
  [define (leading-edge->x edge facing collbox)
    ((if facing fl- fl+) edge (collbox 'r))]
  [define (moving-in? p p-towards)   
    (if p-towards
        (positive? (p 'Vx))
        (negative? (p 'Vx)))]
  [define (away? p p-towards)   
    (and (not (moving-in? p p-towards))
         (not (zero? (p 'Vx))))]
  [define (abs-min v1 v2)
    (if (fl< (flabs v1) (flabs v2))
        v1
        v2)]
  [define p-towards (towards-facing p other)]
  [define other-towards (towards-facing other p)]
  [define x (p 'x)]
  [define Vx (p 'Vx)]
  [define u (other 'x)]
  [define Vu (other 'Vx)]
  [define dest (fl+ x Vx)]  
  [define (push-against)   
    (cond              
      [(and (moving-in? p p-towards)
            (moving-in? other other-towards))
       (fl+ x Vx Vu)]
      [(and (moving-in? p p-towards) (zero? Vu))
       (fl+ x (round (fl/ Vx 2.)))]
      [(and (zero? Vx) (moving-in? other other-towards))
       (fl+ x (round (fl/ Vu 2.)))]
      [(and (moving-in? p p-towards) (away? other other-towards))
       (fl+ x (min Vx Vu))]
      [(or (not (moving-in? p p-towards))
           (not (moving-in? other other-towards)))
       dest])]
  [define (coll p)
    (Hurtbox/keywords
     #:x (p 'x) #:y (p 'y)
     #:r (deep-ref p '(char coll-r))
     #:h (deep-ref p '(char coll-h)))]
  (cond
    [(touch-horiz? (coll p) (coll other))
     (push-against)]
    [(overlap?
      ((coll p) 'x dest)
      ((coll other) 'x (fl+ u Vu)))
     (leading-edge->x      
      (locate-center
       (leading-edge p-towards (coll p))
       (leading-edge (not p-towards) (coll other)))
      p-towards
      (coll p))]
    [else dest]))

(define/contract (turn-towards p other)
  (-> Player? Player? Player?)
  (case (p 'state)
    [(stand block)
     (struct-copy
      Player p
      [facing (towards-facing p other)])]
    [else p]))

(define/contract (block p other)
  (-> Player? Player? Player?)
  (if (and (or (eq? (p 'state) 'stand)
               (eq? (p 'state) 'block))
           ((if (p 'facing) negative? positive?) (horiz-socd p))
           (eq? (other 'state) 'attack)
           ((flabs (fl- (p 'x) (other 'x)))
            . fl< . BLOCK-DISTANCE))
      (struct-copy
       Player p
       [state 'block]
       [Vx 0.])
      p))

(define/contract (locate-boxes p type)
  (-> Player? symbol? (sequenceof (or/c Hurtbox? Hitbox?)))
  (map (branch-upd 'x (curry (if (p 'facing) fl+ fl-) (p 'x))
                   'y (curry fl+ (p 'y)))              
       ((first (p 'anim)) type)))

;move the player, with input from the other player
(define/contract (move p other)
  (-> Player? Player? Player?)
  [define (find-overlaps bs cs)
    (filter (λ(b) (finds (curry overlap? b) cs)) bs)]
  [define hurts (find-overlaps (locate-boxes other 'hit) ;to do: only need first in list
                               (locate-boxes p 'hurt))]  
  
  (cond
    [(not (empty? hurts))
     (struct-copy
      Player p
      [state 'animate]
      [anim (make-hurt-anim
             ((first hurts)
              (if (eq? (p 'state) 'block)
                  'on-block
                  'on-hit))
             (deep-ref p '(fixed colour)))]
      [Vx 0.]
      [Vy 0.])]
    [else
     (case (p 'state)
       [(stand block)
        (struct-copy
         Player p
         [anim (list (standing-anim
                      (deep-ref p '(fixed colour))))]
         [x (collision-move p other)])]
       [(jump)
        (struct-match-copy
         Player p
         [x (collision-move p other)]
         [y (round (fl+ y Vy))]
         [Vy (fl- Vy (deep-ref p '(char gravity)))])]
       [(attack animate)
        (struct-copy
         Player p
         [x (collision-move p other)])])]))

(define (act p other)
  (move
   (block
    (turn-towards
     p other)
    other)
   other))