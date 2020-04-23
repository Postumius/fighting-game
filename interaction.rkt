#lang racket

(provide htbox htbox-x htbox-w  boxes-overlap? locate-collision)

(struct htbox (x y w h))

(define (val-in-range? val r0 rw)
  (and (<= r0 val) (<= val (+ r0 rw))))

(define (ranges-overlap? q0 qw r0 rw)
  (or (val-in-range? q0 r0 rw)
      (val-in-range? (+ q0 qw) r0 rw)
      (val-in-range? r0 q0 qw)))

(define/contract (boxes-overlap? box1 box2)
  (-> htbox? htbox? boolean?)
  (and (ranges-overlap?
        (htbox-x box1) (htbox-w box1)
        (htbox-x box2) (htbox-w box2))
       (ranges-overlap?
        (htbox-y box1) (htbox-h box1)
        (htbox-y box2) (htbox-h box2))))


(define/contract (locate-collision x0 Vx u0 Vu)
  (-> integer? integer? integer? integer? integer?)
  (if (zero? Vx)
      x0
      (round
       (/ (- u0 (* x0 (/ Vu Vx)))
          (- 1 (/ Vu Vx))))))
