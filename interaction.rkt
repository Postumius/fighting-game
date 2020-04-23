#lang racket

(provide
 htbox htbox-x htbox-w
 boxes-overlap? touch-horiz? touch-vert?
 locate-collision locate-center)

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

(define/contract (touch-horiz? box1 box2)
  (-> htbox? htbox? boolean?)
  (and (ranges-overlap?
        (htbox-y box1) (htbox-h box1)
        (htbox-y box2) (htbox-h box2))
       (or
        (= (+ (htbox-x box1) (htbox-w box1))
           (htbox-x box2))
        (= (+ (htbox-x box2) (htbox-w box2))
           (htbox-x box1)))))

(define/contract (touch-vert? box1 box2)
  (-> htbox? htbox? boolean?)
  (and
   (ranges-overlap?
    (htbox-x box1) (htbox-w box1)
    (htbox-x box2) (htbox-w box2))
   (or
    (= (+ (htbox-y box1) (htbox-h box1))
       (htbox-y box2))
    (= (+ (htbox-y box2) (htbox-h box2))
       (htbox-y box1)))))

(define b1 (htbox 0 0 10 10))
(define b2 (htbox 10 10 10 10))


(define/contract (locate-collision x0 Vx u0 Vu)
  (-> integer? integer? integer? integer? integer?)
  (if (zero? Vx)
      x0
      (round
       (/ (- u0 (* x0 (/ Vu Vx)))
          (- 1 (/ Vu Vx))))))

(define/contract (locate-center x0 x)
  (-> integer? integer? integer?)
  (round (+ x0 (/ (- x x0) 2))))
