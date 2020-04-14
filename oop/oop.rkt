#lang racket
(require 2htdp/universe 2htdp/image)
(require "player.rkt")

(struct game-state (p1 p2))

(define-syntax-rule (send-both state method args ...)
  (game-state
   (send (game-state-p1 state) method args ...)
   (send (game-state-p2 state) method args ...)))

(define (draw-players state)
  (match state
    [(game-state p1 p2)
     (place-image
      (send p1 get-model)
      (send p1 get-x)
      (send p1 get-y)
      (place-image
       (send p2 get-model)
       (send p2 get-x)
       (send p2 get-y)
       (empty-scene W H)))]))

(define (move-players state)
  (match state
    [(game-state p1 p2)
     (game-state
      (send p1 move (send p2 get-x))
      (send p2 move (send p1 get-x)))]))  

(define (run-game)
  (big-bang
      (game-state
       (new player%
            [x0 (- (/ W 2) 100)]
            [colour "aquamarine"]
            [left-button "a"]
            [right-button "d"]
            [up-button "w"]
            [med-kick-button "s"])
       (new player%
            [x0 (+ (/ W 2) 100)]
            [colour "medium gray"]
            [left-button "j"]
            [right-button "l"]
            [up-button "i"]
            [med-kick-button "k"]))
    (on-tick move-players 1/60)
    (on-key (λ (state key)
              (send-both state set-key key #t)))              
    (on-release
     (λ (state key) (send-both state set-key key #f)))
    (to-draw draw-players)))