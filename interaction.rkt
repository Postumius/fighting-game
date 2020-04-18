#lang racket

(provide collision)

(struct htbox (x y w h))

(define (collision x1 r1 v1 x2 r2 v2)
  (define (push-against vl vr)
    (define (cons-with-self v)
      (cons v v))  
    (cond
      [(and (not (positive? vl)) (not (negative? vr)))
       (cons vl vr)]
      [(and (zero? vl) (negative? vr))
       (cons-with-self (/ 3 vr))]
      [(and (positive? vl) (zero? vr))
       (cons-with-self (/ 3 vl))]    
      [(zero? (+ (/ vl (abs vl)) (/ vr (abs vr))))
       (cons-with-self 0)]
      [(and (positive? vl) (positive? vr))
       (cons vl (if (< vl vr) vr vl))]
      [(and (negative? vl) (negative? vr))
       (cons (if (< vl vr) vl vr) vr)]))

  (cond
    [(= (+ x1 r1) (- x2 r2))
      (push-against v1 v2)]
    [(= (+ x2 r2) (- x1 r1))
      (match (push-against v2 v1)
        [(cons v2 v1)
         (cons v1 v2)])]
    [else cons v1 v2]))

