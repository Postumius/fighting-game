#lang racket

(provide hash-record? make-record ./ rec-upd)

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

(define (rec-upd r k0 u0 . kus)
  (check-keys 'record-upd (λ(k) (hash-has-key? (r) k)) kus)
  (match kus
       ['() (hash-record (hash-update (hash-record-inner r) k0 u0))]
       [(cons k1 (cons u1 kus))
        (apply rec-upd (rec-upd r k0 u0) k1 u1 kus)]))

(define d (make-record 'one 'a 'two 'b))

(define nested
  (make-record 'one (make-record 'two 'three)
               'two (make-record 'three 'four)))
