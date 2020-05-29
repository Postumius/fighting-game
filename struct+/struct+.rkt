#lang racket

(require (for-syntax syntax/parse racket/syntax)
         syntax/parse/define
         racket/generic data/collection
         "keywords.rkt")

(provide struct+ struct-set deep-ref deep-upd branch-upd)
  
(define-generics symbol-access
  (struct-ref symbol-access sym)
  (struct-set symbol-access sym val))


(define (raise-key-error name keys v)
  (raise-argument-error
   name
   (format "one of ~v"
           keys)
   v))

(define-syntax (struct+ stx)
  (syntax-parse stx
    [(s+ id:identifier (fid:identifier ...))
     #`(begin
         (struct id (fid ...)
           #:transparent
           #:property prop:procedure
           (λ(s k) (struct-ref s k))
           #:methods gen:symbol-access
           [(define (struct-ref s k)
              (case k
                [(syntax->datum fid)
                 (match s [(struct* id ([fid a])) a])] ...
                [else
                 (raise-key-error
                  'struct-ref
                  (list (syntax->datum #'fid) ...) k)]))
            (define (struct-set s k val)
              (case k
                [(syntax->datum fid )
                 (struct-copy id s [fid val])] ...
                [else
                 (raise-key-error
                  'struct-ref
                  (list (syntax->datum #'fid) ...) k)]))])         
         #,(with-syntax
             ([id-field-ids
               (format-id #'id "~a-field-ids" #'id)]
              [id/keywords (format-id #'id "~a/keywords" #'id)])
             #'(begin
                 (define (id-field-ids)
                   (seteq (syntax->datum #'fid) ...))
                 (define/keywords (id/keywords fid ...)
                   (id fid ...)))))]))


(define (single-upd s k f)
  (struct-set s k (f (struct-ref s k))))

(define/contract ((deep-upd ks f) s)
  (-> (listof symbol?) (-> any/c any/c)
      (-> struct? struct?))
  (match ks
    [(cons k '()) (single-upd s k f)]
    [(cons k ks) (single-upd s k (deep-upd ks f))]))

(define/contract ((branch-upd . kfs) s)
  (->* () () #:rest (listof (or/c symbol? (-> any/c any/c)))
       (-> struct? struct?))
  (match kfs
    [(list k f)
     (single-upd s k f)]
    [(list k f kf ...)
     ((apply branch-upd kf) (single-upd s k f))]))

(define/match (deep-ref s keys)
  [(s (list k)) (struct-ref s k)]
  [(s (list k ks ...)) (deep-ref (struct-ref s k) ks)])

(define multi-set (curry foldl (curry apply)))

(define n 10000000)

(struct+ pt (x y z))

(define p (pt 1 2 3))

(define nested (pt (pt 1 2 3) (pt 4 5 6) (pt 7 8 9)))

(struct+ 2p (x y))

(define p2 (2p 1 2))
