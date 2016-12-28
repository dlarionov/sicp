#lang racket

(require sicp-pict)

(define wave-segments
  (list
   (make-segment
    (make-vect 0.006 0.840)
    (make-vect 0.155 0.591))
   (make-segment
    (make-vect 0.006 0.635)
    (make-vect 0.155 0.392))
   (make-segment
    (make-vect 0.304 0.646)
    (make-vect 0.155 0.591))
   (make-segment
    (make-vect 0.298 0.591)
    (make-vect 0.155 0.392))
   (make-segment
    (make-vect 0.304 0.646)
    (make-vect 0.403 0.646))
   (make-segment
    (make-vect 0.298 0.591)
    (make-vect 0.354 0.492))
   (make-segment
    (make-vect 0.403 0.646)
    (make-vect 0.348 0.845))
   (make-segment
    (make-vect 0.354 0.492)
    (make-vect 0.249 0.000))
   (make-segment
    (make-vect 0.403 0.000)
    (make-vect 0.502 0.293))
   (make-segment
    (make-vect 0.502 0.293)
    (make-vect 0.602 0.000))
   (make-segment
    (make-vect 0.348 0.845)
    (make-vect 0.403 0.999))
   (make-segment
    (make-vect 0.602 0.999)
    (make-vect 0.652 0.845))
   (make-segment
    (make-vect 0.652 0.845)
    (make-vect 0.602 0.646))
   (make-segment
    (make-vect 0.602 0.646)
    (make-vect 0.751 0.646))
   (make-segment
    (make-vect 0.751 0.646)
    (make-vect 0.999 0.343))
   (make-segment
    (make-vect 0.751 0.000)
    (make-vect 0.597 0.442))
   (make-segment
    (make-vect 0.597 0.442)
    (make-vect 0.999 0.144))
   (make-segment
    (make-vect 0.395 0.916)
    (make-vect 0.410 0.916))
   (make-segment
    (make-vect 0.376 0.746)
    (make-vect 0.460 0.790))))

(define wave (segments->painter wave-segments))

(define (split f1 f2)
  (lambda(painter n)
    (if (= 0 n)
        painter
        (let ((smaller ((split f1 f2) painter (- n 1))))
          (f1 painter (f2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top-left (up-split painter (- n 1)))
            (bottom-right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

(define (square-of-four tl tr bl br)
  (lambda(painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180 identity flip-horiz)))
    (combine4 (corner-split painter n))))

(paint-hi-res (square-limit wave 3))