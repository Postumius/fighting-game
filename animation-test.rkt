#lang racket

(require
  2htdp/universe 2htdp/image lang/posn
  "./helper-macros.rkt" "./geometry.rkt"
  "./interaction.rkt" racket/promise
  "./record.rkt" data/collection
  "./helper.rkt")

(provide shine place-htboxes draw)

(define (shine colour)
  (repeat-for
   (make-record
    'sprite (place-bottom-left
             (overlay (rotate -90 (triangle 20 "solid" "red"))
                      (rectangle 40 80 "solid" colour))
             40 0
             (rectangle 120 120 "solid" "transparent"))
    'hurt (list (make-record 'x 40 'y 0 'w 40 'h 80
                             'freeze 10 'hitstun 20
                             'pushback '30))
    'hit (list (make-record 'x 30 'y 0 'w 60 'h 90))
    'speed 3)
   10))

(define (place-htboxes boxes colour background)
  [define (box->image b)
    (rectangle (b 'w) (b 'h) "solid" colour)]
  [define (compose img b)
    (place-bottom-left
     (box->image b) (b 'x) (b 'y)
     img)]
  (foldl compose background boxes))

(define/contract (draw anim)
  (-> (sequenceof hash-record?) image?)
  (define frame (first anim))
  (place-htboxes
   (frame 'hit) (color 255 0 0 50)
   (place-htboxes
    (frame 'hurt) (color 0 255 0 50)
    (frame 'sprite))))
  

(define (play-anim anim)
  (big-bang anim
    (on-tick cdr 1)
    (to-draw draw)
    (stop-when empty?)))