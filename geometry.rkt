#lang racket
(require racket/flonum "struct+/struct+.rkt")

(provide overlap? touch-horiz? locate-center)


;helper functions for collision detection
;(struct+ htbox (x y r h))

(define (centers-radiis-relation op b1 b2)
  ((round (flabs (fl- (b1 'x) (b2 'x))))
   . op .
   (round (fl+ (b1 'r) (b2 'r)))))

(define (overlap-y b1 b2)
  (fl<= (fl- (b1 'y) (b2 'h))
        (b2 'y)
        (fl+ (b1 'y) (b1 'h))))

(define/contract (overlap? b1 b2)
  (-> struct? struct? boolean?)
  (and (centers-radiis-relation fl<= b1 b2)
       (overlap-y b1 b2)))

(define/contract (touch-horiz? b1 b2)
  (-> struct? struct? boolean?)
  (and (centers-radiis-relation fl= b1 b2)
       (overlap-y b1 b2)))

(define/contract (locate-center x0 x)
  (-> flonum? flonum? flonum?)
  (fl+ x0 (fl/ (fl- x x0) 2.)))