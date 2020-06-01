#lang racket

(require 2htdp/image
         racket/flonum)

(provide place-bottom place-bottom-center)

(define divide (compose round /))

(define/contract (place-bottom image x y scene)
  (-> image? real? real? image? image?)
  (place-image
   image
   x
   (- (image-height scene) (divide (image-height image) 2) y)
   scene))

(define/contract (place-bottom-center image x y scene)
  (-> image? real? real? image? image?)
  (place-image
   image
   (+ x (divide (image-width scene) 2))
   (- (image-height scene) (divide (image-height image) 2) y)
   scene))