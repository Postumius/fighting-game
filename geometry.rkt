#lang racket
(require 2htdp/image lang/posn "record.rkt")

(provide place-bottom place-bottom-center
         overlap? touch-horiz? locate-center)

(define/contract (place-bottom image x y scene)
  (-> image? real? real? image? image?)
  (place-image
   image
   x
   (- (image-height scene) (/ (image-height image) 2) y)
   scene))

(define/contract (place-bottom-center image x y scene)
  (-> image? real? real? image? image?)
  (place-image
   image
   (+ x (/ (image-width scene) 2))
   (- (image-height scene) (/ (image-height image) 2) y)
   scene))


;collision detection
(define (center-radius-relation op b1 b2)
  ((abs (- (b1 'x) (b2 'x))) . op . (+ (b1 'r) (b2 'r))))

(define (overlap-y b1 b2)
  (<= (- (b1 'y) (b2 'h))
      (b2 'y)
      (+ (b1 'y) (b1 'h))))

(define/contract (overlap? b1 b2)
  (-> hash-record? hash-record? boolean?)
  (and (center-radius-relation <= b1 b2)
       (overlap-y b1 b2)))

(define/contract (touch-horiz? b1 b2)
  (-> hash-record? hash-record? boolean?)
  (and (center-radius-relation = b1 b2)
       (overlap-y b1 b2)))

(define/contract (locate-center x0 x)
  (-> integer? integer? integer?)
  (round (+ x0 (/ (- x x0) 2))))