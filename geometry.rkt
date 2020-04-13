#lang racket
(require 2htdp/image lang/posn)

(provide place-bottom-left rectangles-overlap?)

(define/contract (place-bottom-left image x y scene)
  (-> image? real? real? image? image?)
  (place-image
   image
   (+ x (/ (image-width image) 2))
   (- (image-height scene) (/ (image-height image) 2) y)
   scene))


(define (val-in-range? val r0 rw)
  (and (<= r0 val) (<= val (+ r0 rw))))

(define (ranges-overlap? q0 qw r0 rw)
  (or (val-in-range? q0 r0 rw)
      (val-in-range? (+ q0 qw) r0 rw)
      (val-in-range? r0 q0 qw)))

(define/contract (rectangles-overlap? x1 y1 rec1 x2 y2 rec2)
  (-> real? real? image? real? real? image? boolean?)
  (and (ranges-overlap?
        x1 (image-width rec1) x2 (image-width rec2))
       (ranges-overlap?
        y1 (image-height rec1) y2 (image-height rec2))))
        

(define sc (empty-scene 600 300))

(define bg (rectangle 120 120 "solid" "transparent"))

(define rect (rectangle 40 80 "solid" "orangered"))