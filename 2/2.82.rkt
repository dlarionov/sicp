#lang racket

(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))

(define *coercion-table* (make-hash))
(define (put-coerciont type1 type2 proc) (hash-set! *coercion-table* (list type1 type2) proc))
(define (get-coercion type1 type2) (hash-ref *coercion-table* (list type1 type2) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (cast-args type1 args)
  (define (iter head tail)
    (if (null? tail)
        (reverse head)
        (let ((type2 (type-tag (car tail))))
          (if (eq? type2 type1)
              (iter (cons (car tail) head) (cdr tail))
              (let ((t2->t1 (get-coercion type2 type1)))
                (if (null? t2->t1)
                    '()
                    (iter (cons (t2->t1 (car tail)) head) (cdr tail)))))
          )))    
  (iter '() args))

(define (apply-generic op . args)
  (define (apply-generic-internal local-args)
    (print local-args)
    (newline)
    (let ((type-tags (map type-tag local-args)))
      (let ((proc (get op type-tags)))
        (if (null? proc)
            false
            (apply proc (map contents local-args))))))
  (define (apply-generic-iter set)
    (if (null? set)
        (error "No method for these types")
        (let ((result (apply-generic-internal (car set))))
          (if result
              result
              (apply-generic-iter (cdr set))))))  
  (let ((result (apply-generic-internal args)))
    (if result
        result
        (let ((types (remove-duplicates (map type-tag args))))
          (if (> (length types) 1)              
              (let ((set (filter
                          (lambda(i)(not (null? i)))
                          (map
                           (lambda(i) (cast-args i args))
                           types))))
                (apply-generic-iter set))
              (error "No method for these types"))))))
          
(put-coerciont 't2 't1 (lambda(x) (cons 't1 (cdr x))))
(put-coerciont 't1 't2 (lambda(x) (cons 't2 (cdr x))))
(put-coerciont 't3 't2 (lambda(x) (cons 't2 (cdr x))))
(cast-args 't2 (list (cons 't1 1) (cons 't2 2) (cons 't3 3) (cons 't1 42)))

(put 'add '(t2 t2 t2 t2) (lambda(a b c d) (+ a b c d)))
(apply-generic 'add (cons 't1 1) (cons 't2 2) (cons 't3 3) (cons 't1 42))