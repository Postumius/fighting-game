#lang racket
(require 2htdp/universe 2htdp/image)


(define WIDTH 100)
(define HEIGHT 200)
(define FLOOR (- HEIGHT 20))

(define (draw-a-box state)
  (match state
    [(list x y v)
      (place-image BOX x y
                   (empty-scene WIDTH HEIGHT))]))

(define BOX (rectangle 20 40 "solid" "maroon"))

(define (fall state)
  (match state
    [(list x y v)
     (if (and (>= y FLOOR) (<= v 0))
         (list x FLOOR v)
         (list x (- y v) (- v 0.2)))]))

(define (move state key)
  (match state
    [(list x y v)
     (cond
       [(key=? key "left") (list (sub1 x) y v)]
       [(key=? key "right") (list (add1 x) y v)]
       [(key=? key "up") (list x y 5)])]))

(define (stop state)
  (match state
    [(list x y v)
     (> y FLOOR)]))

(big-bang (list (/ WIDTH 2) FLOOR 0)
  (on-tick fall)
  (on-key move)
  (to-draw draw-a-box))
