#lang racket

(require
  2htdp/universe 2htdp/image lang/posn
  "./helper-macros.rkt" "./geometry.rkt"
  racket/promise "./struct+/struct+.rkt"
  "./helper.rkt"
  "image.rkt"
  racket/flonum)

(provide shine place-htboxes draw make-hurt-anim
         standing-anim
         (struct-out Hurtbox)
         Hurtbox/keywords
         (struct-out On-hit)
         (struct-out Hitbox)
         (struct-out Anim-frame))

(struct+ Hurtbox (x y r h))

(struct+ On-hit (freeze hitstun pushback))
(struct+ Hitbox (x y r h on-hit on-block))

(struct+ Anim-frame (sprite hurt hit speed))

(define (standing-anim colour)
  (Anim-frame/keywords
   #:sprite (place-bottom-center
            (overlay (rotate -90 (triangle 20 "solid" "red"))
                     (rectangle 40 80 "solid" colour))
            0 0
            (rectangle 120 120 "solid" "transparent"))
   #:hurt (list (Hurtbox 0. 0. 20. 80.))
   #:hit empty
   #:speed 0.))

(define (shine colour)
  (repeat-for
   (Anim-frame
    (place-bottom-center
     (overlay (rotate -90 (triangle 20 "solid" "red"))
              (rectangle 40 80 "solid" colour))
     0 0
     (rectangle 120 120 "solid" "transparent"))
    (list (Hurtbox 0. 0. 20. 80.))
    (list (Hitbox/keywords
           #:x 0. #:y 0. #:r 30. #:h 90.
           #:on-hit (On-hit/keywords
                     #:freeze 8.0 #:hitstun 10.0
                     #:pushback 15.0)
           #:on-block (On-hit/keywords
                       #:freeze 8.0 #:hitstun 5.0
                       #:pushback 10.0)))
    3.0)
   10))

(define (lean-back colour)
  (Anim-frame/keywords
   #:sprite (place-bottom-center
             (overlay
              (rotate -90 (triangle 20 "solid" "red"))
              (polygon (list (make-posn 0 0)
                             (make-posn 40 0)
                             (make-posn 50 80)
                             (make-posn 10 80))
                    "solid" colour))
            0 0
            (rectangle 120 120 "solid" "transparent"))
   #:hurt (list (Hurtbox 0. 0. 25. 80.))
   #:hit empty
   #:speed 0.0))

(define (slide-back d t)  
  [define range-t (range 0.0 t 1.0)]
  [define v-scale (fl/ d (apply fl+ range-t))]
  (reverse (map (curry fl* -1.0 v-scale)
                range-t)))

(define chop (compose inexact->exact round))

(define/contract (make-hurt-anim hit colour)
  (-> On-hit? image-color? (listof Anim-frame?))
  (match hit
    [(struct* On-hit ([freeze frz] [hitstun stn] [pushback psh]))     
     [define speeds (slide-back psh stn)]
     (build-list
      (chop (fl+ frz stn))
      (λ(i)
        (cond
          [(i . < . frz)
           (lean-back colour)]
          [else
           (struct-set
            (lean-back colour)
            'speed (list-ref speeds (chop (- i frz))))])))]))


(define (place-htboxes boxes colour background)
  [define (box->image b)
    (rectangle (* 2 (b 'r)) (b 'h) "solid" colour)]
  [define (compose-img b img)
    (place-bottom-center
     (box->image b) (b 'x) (b 'y)
     img)]
  (foldl compose-img background boxes))

(define/contract (draw anim)
  (-> (listof Anim-frame?) image?)
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