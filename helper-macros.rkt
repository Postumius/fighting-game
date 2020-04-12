#lang racket

(provide multi-match)

(define-syntax multi-match
  (syntax-rules ()    
    [(multi-match
      (val-expr0 val-expr1 ...)
      [(pat0 pat1 ...) body ...]
      ...)
     (match (cons val-expr0 val-expr1 ...)
       [(cons pat0 pat1 ...) body ...]
       ...)]))
     