#lang racket
(require 2htdp/universe 2htdp/image)

(define W 400)
(define H 200)
(define F (- H 21))

(struct keys (left right up) #:transparent)

(struct player (x y Vx Vy model
                  inputs state key-map) #:transparent)
(define (make-player x colour key-map)
  (player x F 0 0 (rectangle 20 40 "solid" colour)
          (keys #f #f #f) "stand" key-map))

(define (player-left p) (keys-left (player-inputs p)))
(define (player-right p) (keys-right (player-inputs p)))
(define (player-up p) (keys-up (player-inputs p)))

(define-syntax struct-copy*
  (syntax-rules ()
    [(struct-copy* struct-id id) id]
    [(struct-copy*
      struct-id id
      [field-id0 val0] [field-id1 val1] ...)
     (let ([id
            (struct-copy
             struct-id id
             [field-id0 val0])])
       (struct-copy* struct-id id [field-id1 val1] ...))]))

(define ((press val) p key)
  (define-syntax-rule (set-key p k bool)
  (struct-copy
   player p
   [inputs (struct-copy keys (player-inputs p) [k bool])]))
  (cond
    [(key=? key (keys-left (player-key-map p)))
     (set-key p left val)]
    [(key=? key (keys-right (player-key-map p)))
     (set-key p right val)]
    [(key=? key (keys-up (player-key-map p)))
     (set-key p up val)]
    [else p]))

(define (horiz-socd p)
  (match (player-inputs p)
    [(keys #t #f _) -1]
    [(keys #f #t _) 1]
    [else 0]))

(define (vert-socd p)
  (match (player-inputs p)
    [(keys _ _ #t) 1]
    [else 0]))

(define (move p)
  (case (player-state p)
    [("stand")
     (case (vert-socd p)
       [(1)
        (struct-copy*
         player p
         [Vx (* 2 (horiz-socd p))] [Vy 6] [state "jump"])]
       [else
        (struct-copy*
         player p
         [Vx (* 2 (horiz-socd p))]
         [x (+ (player-x p) (player-Vx p))])])]
    [("jump")
     (if (and (>= (player-y p) F) (<= (player-Vy p) 0))
         (struct-copy* player p [y F] [Vy 0] [state "stand"])
         (struct-copy*
          player p
          [x (+ (player-x p) (player-Vx p))]
          [y (- (player-y p) (player-Vy p))]
          [Vy (- (player-Vy p) 1)]))]))

(define (draw-players p1)
  (place-image
   (player-model p1)
   (player-x p1)
   (player-y p1)
   (empty-scene W H)))

(define (run-game)
  (big-bang
      (make-player
       (/ W 2)
       "aquamarine"
       (keys "a" "d" "w"))
    (on-tick move)
    (on-key (press #t))
    (on-release (press #f))
    (to-draw draw-players)))

