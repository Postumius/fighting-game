#lang racket

(require
  2htdp/universe 2htdp/image lang/posn
  "./helper-macros.rkt" "./geometry.rkt"
  "./interaction.rkt" racket/promise
  "./record.rkt")

(provide simple place-htboxes draw)

(define (cycle val n)
  (build-list n (λ(_) val)))

(define (simple colour)
  (cycle
   (make-record
    'sprite (place-bottom-left
             (overlay (rotate -90 (triangle 20 "solid" "red"))
                      (rectangle 40 80 "solid" colour))
             40 0
             (rectangle 120 120 "solid" "transparent"))
    'hurt (list (make-record 'x 40 'y 0 'w 40 'h 80))
    'hit (list (make-record 'x 30 'y 0 'w 60 'h 90)))
   10))

(define (place-htboxes boxes colour background)
  [define (box->image b)
    (rectangle (b 'w) (b 'h) "solid" colour)]
  [define (compose b img)
    (place-bottom-left
     (box->image b) (b 'x) (b 'y)
     img)]
  (foldl compose background boxes))

(define/contract (draw anim)
  (-> (listof hash-record?) image?)
  (define frame (car anim))
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