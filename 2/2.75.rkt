#lang racket

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'mag) r)
          ((eq? op 'ang) a)
          ((eq? op 'real) (* r (cos a)))
          ((eq? op 'img) (* r (sin a)))
          (else (error "bad op" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define i (make-from-mag-ang 1 60))
(apply-generic 'real i)
(apply-generic 'img i)