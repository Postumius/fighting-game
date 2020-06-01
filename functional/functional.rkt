#lang racket

(require 2htdp/universe 2htdp/image
         racket/flonum
         "../geometry.rkt" "player.rkt"
        "../struct+/struct+.rkt"
        "../image.rkt"
        "../helper-macros.rkt")

(struct+ Game-state (p1 p2))

(define (draw-players state) 
  (match state
    [(Game-state p1 p2)
     (place-bottom-center
      (get-frame p1) (p1 'x) (p1 'y)
      (place-bottom-center
       (get-frame p2) (p2 'x) (p2 'y)
       (empty-scene W 300.)))]))

(define (act-move state)
  (struct-match-copy
   Game-state ((branch-upd 'p1 intent 'p2 intent) state)
   [p1 (move p1 p2)]
   [p2 (move p2 p1)]))

(define ((send-key val) state key)
  ((branch-upd 'p1 (curryr read-key key val)
               'p2 (curryr read-key key val))
   state))

(define (run-game)
  (big-bang
      (Game-state
       (make-player
        #:x -100 #:mk-key "s" #:up-key "w"
        #:left-key "a" #:right-key "d" #:colour "aquamarine")
       (make-player
        #:x 100 #:mk-key "k" #:up-key "i"
        #:left-key "j" #:right-key "l" #:colour "medium gray"))
    (on-tick act-move (fl/ 1. 60.))
    (on-key (send-key #t))
    (on-release (send-key #f))
    (to-draw draw-players)))

(run-game)
  
