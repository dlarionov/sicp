#lang racket

(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (square x) (* x x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (apply proc (map contents args)))))

(define (raise z) (apply-generic 'raise z))

(define (make-integer x)
  ((get 'make 'integer) x))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-number x)
  ((get 'make 'number) x))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))  
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  )

(define (install-rational-package)
  (define (gcd a b)
    (if (= a 0)
        b
        (gcd (remainder b a) a)))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define numer car)
  (define denom cdr) 

  (define (tag x) (attach-tag 'rational x))  
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational) (lambda (x) (make-number (/ (numer x) (denom x)))))
  )

(define (install-number-package)
  (define (tag x) (attach-tag 'number x))
  (put 'make 'number (lambda (x) (tag x)))
  (put 'raise '(number) (lambda (x) (make-complex-from-real-imag x 0)))
  )

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))  
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  )

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
   
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (x y) (tag (make-from-mag-ang x y))))
  )

(install-integer-package)
(install-rational-package)
(install-number-package)
(install-complex-package)

(define x (make-integer 42))
x
(raise x)
(raise (raise x))
(raise (raise (raise x)))
