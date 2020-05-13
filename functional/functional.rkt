#lang racket

(require 2htdp/universe 2htdp/image
         "../geometry.rkt" "player.rkt"
         "../record.rkt" "../struct+/struct+.rkt")

(struct game-state (p1 p2))

(define (draw-players state) 
  (match state
    [(game-state p1 p2)
     (place-bottom-center
      (get-frame p1) (p1 'x) (p1 'y)
      (place-bottom-center
       (get-frame p2) (p2 'x) (p1 'y)
       (empty-scene W 300.)))]))

(define (act-move state)
  (struct-match-copy
   game-state ((upd 'p1 intent 'p2 intent) state)
   [p1 (move p1 p2)]
   [p2 (move p2 p1)]))

(define ((send-key val) state key)
  ((upd 'p1 (curryr read-key key val)
        'p2 (curryr read-key key val))
   state))

(define (run-game)
  (big-bang
      (game-state
       (make-player
        -100. "s" "w" "a" "d" "aquamarine")
       (make-player
        100. "k" "i" "j" "l" "medium gray"))
    (on-tick act-move (fl/ 1 60))
    (on-key (send-key #t))
    (on-release (send-key #f))
    (to-draw draw-players)))

(run-game)
  
