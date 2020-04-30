#lang racket

(require
  2htdp/universe 2htdp/image lang/posn
  "./helper-macros.rkt" "./geometry.rkt"
  "./interaction.rkt" racket/promise
  "./record.rkt" data/collection
  "./helper.rkt")

(provide shine place-htboxes draw make-hurt-anim)

(define (shine colour)
  (repeat-for
   (make-record
    'sprite (place-bottom-left
             (overlay (rotate -90 (triangle 20 "solid" "red"))
                      (rectangle 40 80 "solid" colour))
             40 0
             (rectangle 120 120 "solid" "transparent"))
    'hurt (list (make-record
                 'x 40 'y 0 'w 40 'h 80))
    'hit (list (make-record 'x 30 'y 0 'w 60 'h 90
                            'on-hit (make-record
                                     'freeze 8 'hitstun 10
                                     'pushback 15)))
    'speed 3)
   10))

(define (lean-back colour)
  (make-record
   'sprite (place-bottom-left
            (overlay
             (rotate -90 (triangle 20 "solid" "red"))
             (polygon (list (make-posn 0 0)
                            (make-posn 40 0)
                            (make-posn 50 80)
                            (make-posn 10 80))
                    "solid" colour))
            30 0
            (rectangle 120 120 "solid" "transparent"))
   'hurt (list (make-record 'x 30 'y 0 'w 50 'h 80))
   'hit empty
   'speed 0))

(define (slide-back d t)  
  [define first-t (build-sequence t (λ(i) i))]
  (reverse (map (o round (@ * -1 (/ d (foldl + 0 first-t))))
                first-t)))

(define/contract (make-hurt-anim hit colour)
  (-> hash-record? image-color? (sequenceof hash-record?))
  [define speeds (slide-back (hit 'pushback) (hit 'hitstun))]
  (build-sequence
   (+ (hit 'freeze) (hit 'hitstun))
   (λ(i)
     (cond
       [(i . < . (hit 'freeze))
        (lean-back colour)]
       [else
        ((lean-back colour)
         'speed (nth speeds (- i (hit 'freeze))))]))))

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