#lang racket

(require 2htdp/universe 2htdp/image
         "../geometry.rkt" "player.rkt"
         "../record.rkt")

;(struct game-state (p1 p2))

(define (draw-players state) 
  (place-bottom-left
   (get-frame (state 'p1))
   (./ state 'p1 'x)
   (./ state 'p1 'y)
   (place-bottom-left
    (get-frame (state 'p2))
    (./ state 'p2 'x)
    (./ state 'p2 'y)
    (empty-scene W 300))))

(define (act-move state)
  (define s (rec-upd state 'p1 intent 'p2 intent))
  (s 'p1 (move (s 'p1) (s 'p2))
     'p2 (move (s 'p2) (s 'p1))))

(define ((send-key val) state key)
  (state 'p1 (read-key (state 'p1) key val)
         'p2 (read-key (state 'p2) key val)))

(define (run-game)
  (big-bang
      (make-record
       'p1
       (make-player
        (- (/ W 2) 140) "s" "w" "a" "d" "aquamarine")
       'p2
       (make-player
        (+ (/ W 2) 60) "k" "i" "j" "l" "medium gray"))
    (on-tick act-move 1/60)
    (on-key (send-key #t))
    (on-release (send-key #f))
    (to-draw draw-players)))
  