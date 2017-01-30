#lang racket

(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (square x) (* x x))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))  
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'add '(integer integer integer) (lambda (x y z) (tag (+ x y z))))
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
  
  (define (tag x) (attach-tag 'rational x))  
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational) (lambda (x) (make-number (/ (numer x) (denom x)))))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'add '(rational rational rational) (lambda (x y z) (tag (add-rat z (add-rat x y)))))
  )

(define (install-number-package)
  (define (tag x) (attach-tag 'number x))
  (put 'make 'number (lambda (x) (tag x)))
  (put 'raise '(number) (lambda (x) (make-complex x 0)))
  (put 'add '(number number) (lambda (x y) (tag (+ x y))))
  (put 'add '(number number number) (lambda (x y z) (tag (+ x y z))))
  )

(define (install-complex-package)
  (define (make-from-real-imag x y) (cons x y))  
  (define (real-part z) (car z))  
  (define (imag-part z) (cdr z))  
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  
  (define (tag x) (attach-tag 'complex x))  
  (put 'make 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'add '(complex complex complex) (lambda (z1 z2 z3) (tag (add-complex z3 (add-complex z1 z2)))))
  )

(install-integer-package)
(install-rational-package)
(install-number-package)
(install-complex-package)
  
(define (make-integer x) ((get 'make 'integer) x))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-number x) ((get 'make 'number) x))
(define (make-complex x y) ((get 'make 'complex) x y))

(define tower
  '((integer 1)
    (rational 2)
    (number 3)
    (complex 4)))

(define (tower-index x)
  (define (iter tail)
    (if (null? tail)
        -1
        (let ((i (car tail)))
          (if (eq? (car i) x)
              (car (cdr i))
              (iter (cdr tail))))))
  (iter tower))

(define (max-type types)
  (define (iter guess tail)
    (if (null? tail)
        guess
        (if (> (tower-index guess) (tower-index (car tail)))
            (iter guess (cdr tail))
            (iter (car tail) (cdr tail)))))
  (iter (car types) (cdr types)))

(define (raise-to type x)
  (if (eq? (type-tag x) type)
      x
      (raise-to type (raise x))))

(define (raise-args type args)
  (map (lambda(x) (raise-to type x)) args))

(define (apply-generic op . args)  
  (define (apply-generic-internal local-args)
    (let ((type-tags (map type-tag local-args)))
      (let ((proc (get op type-tags)))
        (if (null? proc)
            false
            (apply proc (map contents local-args))))))
  (let ((result1 (apply-generic-internal args)))
    (if result1
        result1
        (let ((types (remove-duplicates (map type-tag args))))
          (if (> (length types) 1)              
              (let ((result2 (apply-generic-internal (raise-args (max-type types) args))))
                (if result2
                    result2
                    (error "No method for these types")))
              (error "No method for these types"))))))

(define (raise z) (apply-generic 'raise z))

(apply-generic 'add (make-integer 1) (make-complex 1 3) (make-rational 1 3))




