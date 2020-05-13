#lang racket

(require
  2htdp/universe 2htdp/image lang/posn
  "./helper-macros.rkt" "./geometry.rkt"
  racket/promise "./struct+/struct+.rkt"
  data/collection "./helper.rkt"
  "image.rkt")

(provide animation shine place-htboxes draw make-hurt-anim)

(struct+ hurtbox x y r h)

(struct+ on-hit)
(struct+ hitbox (x y r h freeze hitstun pushback))

(struct+ anim-frame (sprite hurt hit speed))

(define (shine colour)
  (repeat-for
   (anim-frame
    (place-bottom-center
     (overlay (rotate -90 (triangle 20 "solid" "red"))
              (rectangle 40 80 "solid" colour))
     0 0
     (rectangle 120 120 "solid" "transparent"))
    (list (htbox 0 0 20 80))
    (list (htbox 0 0 30 90
                       'on-hit (make-record
                                'freeze 8 'hitstun 10
                                'pushback 15)))
    3)
   10))

(define (lean-back colour)
  (make-record
   'sprite (place-bottom-center
            (overlay
             (rotate -90 (triangle 20 "solid" "red"))
             (polygon (list (make-posn 0 0)
                            (make-posn 40 0)
                            (make-posn 50 80)
                            (make-posn 10 80))
                    "solid" colour))
            0 0
            (rectangle 120 120 "solid" "transparent"))
   'hurt (list (make-record 'x 0 'y 0 'r 25 'h 80))
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
    (rectangle (* 2 (b 'r)) (b 'h) "solid" colour)]
  [define (compose img b)
    (place-bottom-center
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