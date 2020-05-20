#lang racket

(require (for-syntax
             syntax/parse))

(provide define/keywords)

(define-for-syntax syntax->keyword
  (compose string->keyword symbol->string syntax->datum))

(define-for-syntax (args-add-keywords args)
  (for/fold ([ls null]) ([arg (in-list args)])
    (cons (syntax->keyword arg) (cons arg ls))))

(define-syntax (define/keywords stx)
  (syntax-parse stx
    [(_ (id args ...) body)
     (with-syntax
         ([keyws-args
           (args-add-keywords (syntax->list #'(args ...)))])
       #`(define #,(cons #'id #'keyws-args) body))]))
