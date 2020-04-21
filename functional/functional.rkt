#lang racket

(require 2htdp/universe 2htdp/image
         "../geometry.rkt" "player.rkt")

(struct game-state (p1 p2))

(define (draw-players state)
  (match state
    [(game-state p1 p2)
     (place-bottom-left
      (get-frame p1)
      (- (player-x p1) (/ 120 2))
      (player-y p1)
      (place-bottom-left
       (get-frame p2)
       (- (player-x p2) (/ 120 2))
       (player-y p2)
       (empty-scene W 300)))]))

(define (act-move state)
  (match (game-state (act (game-state-p1 state))
                     (act (game-state-p2 state)))
    [(game-state p1 p2)
     (game-state (move p1 p2) (move p2 p1))]))

(define ((send-key val) state key)
  (match state
  [(game-state p1 p2)
   (game-state (read-key p1 key val) (read-key p2 key val))]))

(define (run-game)
  (big-bang
      (game-state
       (make-player
        (- (/ W 2) 100) "s" "w" "a" "d" "aquamarine")
       (make-player
        (+ (/ W 2) 100) "k" "i" "j" "l" "medium gray"))
    (on-tick act-move 1/60)
    (on-key (send-key #t))
    (on-release (send-key #f))
    (to-draw draw-players)))
  