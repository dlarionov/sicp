#lang racket

(define *table (make-hash))
(define (put op type proc) (hash-set! *table (list op type) proc))
(define (get op type) (hash-ref *table (list op type) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (apply-generic op . args)  
  (define (apply-generic-internal local-args)
    (let ((type-tags (map type-tag local-args)))
      (let ((proc (get op type-tags)))
        (if (null? proc)
            '()
            (apply proc (map contents local-args))))))
  (let ((result (apply-generic-internal args)))
    (if (null? result)
        (error "Method not found" (cons op (map type-tag args)))
        result)))

(define (add x y) (if (and (number? x) (number? y)) (+ x y) (apply-generic 'add x y)))
(define (sub x y) (if (and (number? x) (number? y)) (- x y) (apply-generic 'sub x y)))
(define (mul x y) (if (and (number? x) (number? y)) (* x y) (apply-generic 'mul x y)))
(define (div x y) (if (and (number? x) (number? y)) (/ x y) (apply-generic 'div x y)))
(define (zero? x) (if (number? x) (= x 0) (apply-generic 'zero? x)))
(define (negate x) (if (number? x) (- x) (apply-generic 'negate x)))
(define (nod x y) (if (and (number? x) (number? y)) (gcd x y) (apply-generic 'nod x y)))

(define (install-rational-package)  
  (define (make-rat n d) (let ((g (nod n d))) (cons (div n g) (div d g))))
  (define numer car)
  (define denom cdr)
  
  (define (add-rat x y) (make-rat (add (mul (numer x) (denom y)) (mul (numer y) (denom x))) (mul (denom x) (denom y))))
  (define (sub-rat x y) (make-rat (sub (mul (numer x) (denom y)) (mul (numer y) (denom x))) (mul (denom x) (denom y))))
  (define (mul-rat x y) (make-rat (mul (numer x) (numer y)) (mul (denom x) (denom y))))
  (define (div-rat x y) (make-rat (mul (numer x) (denom y)) (mul (denom x) (numer y))))
 
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'zero? '(rational) (lambda (x) (zero? (numer x))))
  (put 'negate '(rational) (lambda (x) (tag (make-rat (negate (numer x)) (denom x)))))
  (put 'nod '(rational rational) (lambda (x y) (error "Method not implemented")))
  )

(define (install-polynomial-package)    
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term terms) (if (zero? (coeff term)) terms (cons term terms)))
  (define (first-term terms) (car terms))
  (define (rest-terms terms)(cdr terms))
    
  (define (make-poly variable terms) (cons variable terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (add-terms L1 L2)
    (cond ((null? L1) L2)
          ((null? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))            
             (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term
                          (make-term (order t1) (add (coeff t1) (coeff t2)))
                          (add-terms (rest-terms L1) (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (null? L1)
        '()
        (add-terms 
         (mul-term-by-all-terms (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (null? L)
        '()
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-terms L1 L2)
    ;(print (list 'div L1 L2))(newline)
    (if (null? L1)
        (list '() '())
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list '() L1)
              (let ((new-coeff (div (coeff t1) (coeff t2)))
                    (new-order (- (order t1) (order t2))))                
                (let ((mul-result (mul-term-by-all-terms (make-term new-order (negate new-coeff)) L2)))
                  (let ((rest-of-result (div-terms (add-terms L1 mul-result) L2)))
                    (list
                     (adjoin-term (make-term new-order new-coeff) (car rest-of-result))
                     (cadr rest-of-result)))))))))

  (define (pseudoremainder-terms L1 L2)
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (let ((O1 (order t1))
            (O2 (order t2))
            (C2 (coeff t2)))      
        (let ((factor (expt C2 (+ 1 O1 (- O2)))))
          (print factor)(newline)
          (cadr (div-terms                 
                 (mul-term-by-all-terms
                  (make-term 0 factor) L1)
                 L2))))))

  (define (nod-terms L1 L2)
    ;(print (list 'nod-terms L1 L2))(newline)
    (if (null? L2)
        L1
        (nod-terms L2 (pseudoremainder-terms L1 L2))))  

  (define (reduce-terms L)
    (let ((factor (apply gcd (map coeff L))))
      (map (lambda(i) (make-term (order i) (/ (coeff i) factor))) L)))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
        (error "Variables are not the same")))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
        (error "Variables are not the same")))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((quotient-and-remainder (div-terms (term-list p1) (term-list p2))))          
          (list
           (make-poly (variable p1) (car quotient-and-remainder))
           (make-poly (variable p1) (cdr quotient-and-remainder))))
        (error "Variables are not the same")))

  (define (nod-polys p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (reduce-terms (nod-terms (term-list p1) (term-list p2))))
        (error "Variables are not the same")))
  
  (define (negate-poly p)
    (make-poly
     (variable p)
     (map
      (lambda(x) (make-term (order x) (negate (coeff x))))
      (term-list p))))

  (define (tag x) (attach-tag 'polynomial x))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))  
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (let ((result (div-poly p1 p2))) (list (tag (car result)) (tag (cadr result))))))
  (put 'zero? '(polynomial) (lambda (x) (null? x)))
  (put 'negate '(polynomial) (lambda (x) (tag (negate-poly x))))
  (put 'nod '(polynomial polynomial) (lambda (p1 p2) (tag (nod-polys p1 p2))))
  )

(install-rational-package)
(install-polynomial-package)

(define (make-rat n d) ((get 'make 'rational) n d))
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

(define p1 (make-polynomial 'x '((2 1) (1 -1) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 1))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))
(nod (mul p1 p2) (mul p1 p3))
p1