#lang racket
(require 2htdp/image lang/posn)



(provide place-bottom-left boxes-overlap?)

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

(struct htbox (x y w h))
(define/contract (boxes-overlap? box1 box2)
  (-> htbox? htbox? boolean?)
  (and (ranges-overlap?
        (htbox-x box1) (htbox-w box1)
        (htbox-x box2) (htbox-w box2)
       (ranges-overlap?
        (htbox-y box1) (htbox-h box1)
        (htbox-y box2) (htbox-h box2)))))
        

(define sc (empty-scene 600 300))

(define bg (rectangle 120 120 "solid" "transparent"))

(define rect (rectangle 40 80 "solid" "orangered"))