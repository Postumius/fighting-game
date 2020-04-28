#lang racket

(provide hash-record? make-record ./ record-upd)

(define-syntax-rule (check-keys who pred-datum keys-vals)
  (begin
    [define (rec kvs)
      (match kvs
        ['() (void)]
        [(cons k0 (cons v0 kv))
         (if (pred-datum k0)
             (rec kv)
             (raise-argument-error
              who (format "~a" 'pred-datum) k0))]
        [_ (raise-argument-error
            who "a list of even length" keys-vals)])]
  (rec keys-vals)))
 
(struct hash-record (inner)
  #:property prop:procedure
  (case-lambda
    [(r) (hash-record-inner r)]
    [(r key)
     (if (symbol? key)
         (hash-ref (hash-record-inner r) key)
         (raise-argument-error 'record-ref "symbol?" key))]
    [(r . kvs)
     (check-keys 'record-set (λ(k) (hash-has-key? (r) k)) kvs)
     (hash-record (apply hash-set* (r) kvs))]))

(define (make-record . kvs)
  (check-keys 'make-record symbol? kvs)
  (hash-record (apply hasheq kvs)))

(define/contract (./ r k . ks)
  (->* (hash-record? any/c) #:rest list? any/c)
  (if (empty? ks)
      (r k)
      (apply ./ (r k) (car ks) (cdr ks))))

(define/contract ((record-upd k0 u0 . kus) r)
  (([or/c symbol? (listof symbol?)] [-> any/c any/c])
   #:rest list? . ->* . (-> hash-record? hash-record?))
  (match kus
    ['()
     (match k0
       ['() r]
       [(cons k '()) ((record-upd k u0) r)]
       [(cons k ks) ((record-upd k (record-upd ks u0)) r)]
       [_
        (hash-record
         (hash-update (hash-record-inner r) k0 u0))])]
    [(cons k1 (cons u1 kus))
     ((apply record-upd k1 u1 kus) ((record-upd k0 u0) r))]))

(define ((deep-upd keys updater) r)
  (match keys
    ['() r]
    [(cons k '()) (record-upd r k updater)]
    [(cons k ks) (record-upd r k (deep-upd ks updater))]))

(define d (make-record 'one 1 'two 2))

(define nested
  (make-record 'one (make-record 'two 'three)
               'two (make-record 'three 'four)))
