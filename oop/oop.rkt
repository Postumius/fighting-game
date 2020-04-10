#lang racket
(require 2htdp/universe 2htdp/image)
(require "player.rkt")

(define (draw-players p1)
  (place-image
   (send p1 get-model)
   (send p1 get-x)
   (send p1 get-y)
   (empty-scene W H)))

(define (run-game)
  (big-bang
      (new player%
           [x0 (/ W 2)]
           [colour "aquamarine"]
           [move-left "left"]
           [move-right "right"]
           [jump "up"])
    (on-tick (λ (p) (send p move)))
    (on-key (λ (p key) (send p set-key key #t)))
    (on-release (λ (p key) (send p set-key key #f)))
    (to-draw draw-players)))