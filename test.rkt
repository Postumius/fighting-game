#lang racket/gui

(require pict racket/draw racket/contract)

(define/contract (doub n)
  (-> number? number?)
  (+ n n))

(define dude (filled-rectangle 40 80 #:color "aquamarine"))

(define person (filled-rectangle 40 80 #:color "white"))

(define frame
  (new frame%
       [label "example"]
       [width 300]
       [height 300]))

(define msg (new message% [parent frame]
                 [label "I dunno"]))

