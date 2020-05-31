#lang racket

(require (for-syntax
             syntax/parse
             racket/match
             racket/list
             racket/function
             racket/syntax))

(provide define/keywords define/contract/keywords)

(define-for-syntax syntax->keyword
  (compose string->keyword symbol->string syntax->datum))

(define-for-syntax (args->keywords args)
  (for/list ([arg (in-list args)])
    (syntax->keyword arg)))

(define-for-syntax (zip-with f xs ys)
    (match* (xs ys)
      [('() ys) ys]
      [(xs '()) xs]
      [((cons x xs) (cons y ys))
       (cons (f x y) (zip-with f xs ys))]))

(define-for-syntax intersperse
  (compose flatten (curry zip-with cons)))

(define-syntax (define/keywords stx)
  (syntax-parse stx
    [(_ (f:id arg:id ...) body:expr ...)
     (with-syntax*
         ([keyws (args->keywords (syntax->list #'(arg ...)))]
          [keyws-args
           (intersperse (syntax->list #'keyws)
                        (syntax->list #'(arg ...)))])
       #`(define #,(cons #'f #'keyws-args)
           body ...))]))

(define-syntax (define/contract/keywords stx)
  (syntax-parse stx
    [(_ (f:id arg:id ...)
        (contract:id pred:expr ...)
        body:expr ...)
     (with-syntax*
         ([keyws (args->keywords (syntax->list #'(arg ...)))]
          [keyws-args
           (intersperse (syntax->list #'keyws)
                        (syntax->list #'(arg ...)))]
          [keyws-preds
           (intersperse (syntax->list #'keyws)
                        (syntax->list #'(pred ...)))])
       #`(define/contract #,(cons #'f #'keyws-args)
           #,(cons #'contract #'keyws-preds)
           body ...))]))

