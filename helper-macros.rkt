#lang racket

(provide multi-match struct-copy*)

(define-syntax multi-match
  (syntax-rules ()    
    [(multi-match
      (val-expr0 val-expr1 ...)
      [(pat0 pat1 ...) body ...]
      ...)
     (match (cons val-expr0 val-expr1 ...)
       [(cons pat0 pat1 ...) body ...]
       ...)]))

(define-syntax struct-copy*
  (syntax-rules ()
    [(struct-copy* struct-id id) id]
    [(struct-copy*
      struct-id id
      [field-id0 val0] [field-id1 val1] ...)
     (let ([id
            (struct-copy
             struct-id id
             [field-id0 val0])])
       (struct-copy* struct-id id [field-id1 val1] ...))]))

