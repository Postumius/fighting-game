#lang racket

(define (make-hsh . ls)
  (define inner (apply hasheq ls))
  (case-lambda    
      [() inner]
      [(key) (hash-ref inner key)]
      [(key v) (make-hsh (hash-set inner key v))]))

(define h2 (make-hsh 'one 1 'two 2))

(define (has-new-key? hsh keys-vals)
  (define (rec kvs)
    (match kvs
      ['() #f]
      [(cons k0 (cons v0 kv))
       (if (hash-has-key? hsh k0)
           (rec kv)
           (list k0))]
      [_ (raise-argument-error
          'has-new-key?
          (format "~a" '(λ (ls) (even? (length ls))))
          keys-vals)]))
  (rec keys-vals))

(struct record (inner)
  #:property prop:procedure
  (case-lambda
    [(r) (record-inner r)]
    [(r key) (hash-ref (record-inner r) key)]
    [(r . kvs)
     (define new-key (has-new-key? (r) kvs))
     (if new-key
         (raise-argument-error
          'record "a valid key"
          (car new-key))
         (record (apply hash-set* (r) kvs)))]))

(define/contract (make-record . ls)
  (->* () #:rest list? record?)
  (record (apply hasheq ls)))

(define/contract (./ r k . ks)
  (->* (record? any/c) #:rest list? any/c)
  (if (empty? ks)
      (r k)
      (apply ./ (r k) (car ks) (cdr ks))))

(define (rec-upd r k0 u0 . kus)
 (match kus
       ['() (record (hash-update (record-inner r) k0 u0))]
       [(cons k1 (cons u1 kus))
        (apply rec-upd (rec-upd r k0 u0) k1 u1 kus)]
       [_ (raise-argument-error
           'rec-upd
           (format "~a" '(λ (ls) (even? (length ls))))
           kus)]))

(define d (make-record 1 'a 2 'b))

(define r (make-record "1" 1 "2" 2))

(define nested
  (make-record 1 (make-record 2 'three)
               2 (make-record 3 'four)))
