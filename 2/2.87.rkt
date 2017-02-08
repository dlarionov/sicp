#lang racket
(define (square x) (* x x))

(define table (make-hash))
(define (put op type proc) (hash-set! table (list op type) proc))
(define (get op type) (hash-ref table (list op type) '()))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'integer)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (if (and (number? datum) (= (round datum) datum))
      'integer
      (car datum)))
(define (contents datum)
  (if (and (number? datum) (= (round datum) datum))
      datum
      (cdr datum)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (minus x) (apply-generic 'minus x))

(define (raise z) (apply-generic 'raise z))
(define (raise-index x)
  (define (iter i type guess)
    (let ((guess-type (type-tag guess)))
      (if (eq? type guess-type)
          i
          (iter (+ i 1) guess-type (raise guess)))))
  (iter 0 (type-tag x) (raise x)))
(define (raise-to-index i x)
  (if (= (raise-index x) i)
      x
      (raise-to-index i (raise x))))
(define (raise-args args)
  (let ((i (apply min (map raise-index args))))
    (map (lambda(x) (raise-to-index i x)) args)))

(define (project z) (apply-generic 'project z))
(define (drop z)
  (let ((p (project z)))
    (if (eq? (type-tag z) (type-tag p))
        z
        (drop p))))

(define (apply-generic op . args)  
  (define (apply-generic-internal local-args)
    (let ((type-tags (map type-tag local-args)))
      (let ((proc (get op type-tags)))
        (if (null? proc)
            '()
            (apply proc (map contents local-args))))))
  (let ((result1 (apply-generic-internal args)))
    (if (null? result1)
        (let ((types (remove-duplicates (map type-tag args))))
          (if (> (length types) 1)              
              (let ((result2 (apply-generic-internal (raise-args args))))
                (if (null? result2)                    
                    (error "No method for these types" (cons op args))
                    result2))
              (error "No method for these types" (cons op args))))
        result1)))

(define (make-integer x) ((get 'make 'integer) x))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real x) ((get 'make 'real) x))
(define (make-complex x y) ((get 'make-from-real-imag 'complex) x y))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (make-rational x y)))
  (put 'equ? '(integer integer) (lambda (x y) (= x y)))
  (put 'zero? '(integer) (lambda (x) (= x 0)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'project '(integer) (lambda (x) (tag x)))
  (put 'minus '(integer) (lambda (x) (tag (- x))))
  )

(define (install-rational-package)
  (define (gcd a b) (if (= a 0) b (gcd (remainder b a) a)))
  (define (make-rat n d) (let ((g (gcd n d))) (cons (/ n g) (/ d g))))
  (define numer car)
  (define denom cdr)
  
  (define (add-rat x y) (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (sub-rat x y) (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (mul-rat x y) (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
  (define (div-rat x y) (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
 
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) (lambda (x y) (and (= (numer x) (numer y))(= (denom x) (denom y)))))
  (put 'zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'project '(rational) (lambda (x)
                              (cond ((= (numer x) 0) (make-integer 0))
                                    ((= (denom x) 1) (make-integer (numer x)))
                                    (else (tag x)))))
  (put 'minus '(rational) (lambda (x) (mul -1 (tag x))))
  )

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put 'zero? '(real) (lambda (x) (= x 0)))
  (put 'raise '(real) (lambda (x) (make-complex x 0)))
  (put 'project '(real) (lambda(x)
                          (if (= (round x) x)
                              (make-integer x)
                              (tag x))))
  (put 'minus '(real) (lambda (x) (tag (- x))))
  )

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
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  )

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
  
  (define (tag x) (attach-tag 'polar x))
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)  
  )

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2) (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
  
  (define (tag x) (attach-tag 'complex x))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'equ? '(complex complex) (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))
  (put 'zero? '(complex) (lambda (x) (= (magnitude x) 0)))
  (put 'angle '(complex) angle)
  (put 'raise '(complex) (lambda (x) (tag x)))
  (put 'project '(complex) (lambda (x)
                             (if (= (imag-part x) 0)
                                 (make-real (real-part x))
                                 (tag x))))
  (put 'minus '(complex) (lambda (x) (mul -1 (tag x))))
  )

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)

(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (adjoin-term term term-list)
    (if (zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term
                          (make-term (order t1) (add (coeff t1) (coeff t2)))
                          (add-terms (rest-terms L1) (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms 
         (mul-term-by-all-terms (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
        (error "variables are not the same")))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
        (error "variables are not the same")))

  (define (minus-poly p) (make-poly (variable p) (map
                                                  (lambda(x) (make-term (order x) (minus (coeff x))))
                                                  (term-list p))))
  
  (define (tag x) (attach-tag 'polynomial x))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (minus-poly p2)))))
  (put 'zero? '(polynomial) (lambda (x) (empty-termlist? (term-list x)))) ; TODO poly is null when all coeffs are null.
  (put 'minus '(polynomial) (lambda (x) (tag (minus-poly x))))
  )

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define x1 (make-polynomial
            'x
            (list
             (list 2 3)
             (list 1 (make-complex 2 3))
             (list 0 7))))

(define x2 (make-polynomial
            'x
            (list
             (list 4 1)
             (list 2 (make-rational 2 3))
             (list 0 (make-complex 5 3)))))

(add x1 x2)
(mul x1 x2)
(sub x1 x2)

(define y1 (make-polynomial 'x
                            (list
                             (list 2 (make-polynomial 'y (list (list 1 1) (list 0 1))))
                             (list 1 (make-polynomial 'y (list (list 2 1) (list 0 1))))
                             (list 0 (make-polynomial 'y (list (list 1 1) (list 0 -1)))))))

(define y2 (make-polynomial 'x
                            (list
                             (list 1 (make-polynomial 'y (list (list 1 1) (list 0 -2))))
                             (list 0 (make-polynomial 'y (list (list 3 1) (list 0 7)))))))

(add y1 y2)
(mul y1 y2)
(sub y1 y2)