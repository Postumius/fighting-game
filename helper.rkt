#lang racket

(provide o @)

(define/contract (o f . fs)
  ((procedure?) #:rest (listof procedure?) . ->* . procedure?)
  (if (equal? fs empty)
      (λ args (apply f args))
      (λ args
        (f (apply (apply o fs) args)))))

(define/contract (@ f . args)
  ((procedure?) #:rest list? . ->* . procedure?)
  (λ rest (apply f (append args rest))))