#lang racket

(require "./record.rkt")

(provide
 boxes-overlap? touch-horiz?
 locate-collision locate-center)

;(struct htbox (x y w h))

(define (val-in-range? val r0 rw)
  (and (<= r0 val) (<= val (+ r0 rw))))

(define (ranges-overlap? q0 qw r0 rw)
  (or (val-in-range? q0 r0 rw)
      (val-in-range? (+ q0 qw) r0 rw)
      (val-in-range? r0 q0 qw)))

(define/contract (boxes-overlap? box1 box2)
  (-> hash-record? hash-record? boolean?)
  (and (ranges-overlap?
        (box1 'x) (box1 'w)
        (box2 'x) (box2 'w))
       (ranges-overlap?
        (box1 'y) (box1 'h)
        (box2 'y) (box2 'h))))

(define/contract (touch-horiz? box1 box2)
  (-> hash-record? hash-record? boolean?)
  (and (ranges-overlap?
        (box1 'y) (box1 'h)
        (box2 'y) (box2 'h))
       (or
        (= (+ (box1 'x) (box1 'w))
           (box2 'x))
        (= (+ (box2 'x) (box2 'w))
           (box1 'x)))))


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
