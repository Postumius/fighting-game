#lang racket

(require data/collection)

(provide o @ repeat-for finds)

(define/contract (o f . fs)
  ((procedure?) #:rest (listof procedure?) . ->* . procedure?)
  (if (equal? fs empty)
      (λ args (apply f args))
      (λ args
        (f (apply (apply o fs) args)))))

(define/contract (@ f . args)
  ((procedure?) #:rest list? . ->* . procedure?)
  (λ rest (apply f (append args rest))))

(define (repeat-for val n)
  (build-sequence n (λ(_) val)))

;WARNING: do not use on infinite sequence
(define (finds pred seq)
  (match seq
    [(sequence) #f]
    [(sequence head tail ...)
     (if (pred head) head (finds pred tail))]))