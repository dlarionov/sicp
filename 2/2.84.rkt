#lang racket

(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (square x) (* x x))

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

(define (raise-args type args)
  args)

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))  
  (put 'make 'integer (lambda (x) (tag x)))  
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
  (put 'numer 'rational numer)
  (put 'denom 'rational denom) 
  )

(define (install-number-package)
  (define (tag x) (attach-tag 'number x))
  (put 'make 'number (lambda (x) (tag x)))  
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

(define (install-tower-package)
  (install-integer-package)
  (install-rational-package)
  (install-number-package)
  (install-complex-package)
  
  (define (make-integer x) ((get 'make 'integer) x))
  (define (make-rational n d) ((get 'make 'rational) n d))
  (define (numer z) ((get 'numer 'rational) z))
  (define (denom z) ((get 'denom 'rational) z))
  (define (make-number x) ((get 'make 'number) x))
  (define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
  (define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))  
  
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'raise '(rational) (lambda (x) (make-number (/ (numer x) (denom x)))))
  (put 'raise '(number) (lambda (x) (make-complex-from-real-imag x 0)))
  
  ;(raise (raise (raise (make-integer 42))))
  )

(install-tower-package)

(define tower '(integer rational number complex))

(define (index-tower tower)
  (define (iter i result tail)
    (if (null? tail)
        result
        (iter (+ 1 i) (cons (cons i (car tail)) result) (cdr tail))))
  (iter 0 '() tower))

; bad method
(define (get-index type)
  (define (iter tail)
    (if (null? tail)
        (- 0 1)
        (let ((x (car tail)))
          (if (eq? (cdr x) type)
              (car x)
              (iter (cdr tail))))))
  (let ((set (index-tower tower)))
    (iter set)))

(define (max-type types)
  (define (iter max tail)
    (if (null? tail)
        max
        (if (> (get-index max) (get-index (car tail)))
            (iter max (cdr tail))
            (iter (car tail) (cdr tail)))))
  (iter (car types) (cdr types)))

(max-type '(complex number integer))


  