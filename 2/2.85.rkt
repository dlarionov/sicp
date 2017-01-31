#lang racket

(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (square x) (* x x))

(define (make-integer x) ((get 'make 'integer) x))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real x) ((get 'make 'real) x))
(define (make-complex x y) ((get 'make 'complex) x y))

(define (install-integer-package)
  (define (project z) (make-integer z))
  
  (define (tag x) (attach-tag 'integer x))  
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'add '(integer integer integer) (lambda (x y z) (tag (+ x y z))))
  (put 'project '(integer) project)
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
  
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (project z)
    (cond ((= (numer z) 0) (make-integer 0))
          ((= (denom z) 1) (make-integer (numer z)))
          (else (make-rational (numer z) (denom z)))))
  
  (define (tag x) (attach-tag 'rational x))  
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'add '(rational rational rational) (lambda (x y z) (tag (add-rat z (add-rat x y)))))
  (put 'project '(rational) project)
  )

(define (install-real-package)
  (define (project z)
    (if (= (round z) z)
        (make-integer z)
        (make-real z)))
  
  (define (tag x) (attach-tag 'real x))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'raise '(real) (lambda (x) (make-complex x 0)))
  (put 'add '(real real real) (lambda (x y z) (tag (+ x y z))))
  (put 'project '(real) project)
  )

(define (install-complex-package)
  (define (make-from-real-imag x y) (cons x y))  
  (define (real-part z) (car z))  
  (define (imag-part z) (cdr z))  
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (project z)
    (if (= (imag-part z) 0)
        (make-real (real-part z))
        (make-complex (real-part z) (imag-part z))))
  
  (define (tag x) (attach-tag 'complex x))  
  (put 'make 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'add '(complex complex complex) (lambda (z1 z2 z3) (tag (add-complex z3 (add-complex z1 z2)))))
  (put 'project '(complex) project)
  )

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
  
(define (apply-generic op . args)  
  (define (apply-generic-internal local-args)
    (let ((type-tags (map type-tag local-args)))
      (let ((proc (get op type-tags)))
        (if (null? proc)
            false
            (apply proc (map contents local-args))))))
  (let ((result (apply-generic-internal args)))
    (print (cons op args))
    (newline)
    (if result
        result
        (error "No method for these types"))))

(define (raise z) (apply-generic 'raise z))
(define (project z) (apply-generic 'project z))

(define (drop z)
  (let ((p (project z)))
    (if (eq? (type-tag z) (type-tag p))
        z
        (drop p))))

(drop (make-complex 1.1 0))
(apply-generic 'add (make-complex 1 -2) (make-complex 1.5 -2) (make-complex 1.5 4))




