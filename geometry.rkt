#lang racket
(require 2htdp/image lang/posn)

(provide place-bottom-left)

(define/contract (place-bottom-left image x y scene)
  (-> image? real? real? image? image?)
  (place-image
   image
   (+ x (/ (image-width image) 2))
   (- (image-height scene) (/ (image-height image) 2) y)
   scene))


(define sc (empty-scene 600 300))

(define bg (rectangle 120 120 "solid" "transparent"))

(define rect (rectangle 40 80 "solid" "orangered"))