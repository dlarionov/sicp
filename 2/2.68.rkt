#lang racket

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))

(define (contains? x arr)
  (pair?
   (filter
    (lambda(i) (eq? i x))
    arr)))

(define (encode-symbol x tree)
  (if (leaf? tree)
      '()
      (let ((l (left-branch tree))
            (r (right-branch tree)))
        (let ((ls (symbols l))
              (rs (symbols r)))         
            (cond ((contains? x ls) (cons 0 (encode-symbol x l)))
                  ((contains? x rs) (cons 1 (encode-symbol x r)))
                  (else (error "bad x" x)))))))

(define sample-tree
  (make-tree 
   (make-leaf 'A 4)
   (make-tree
    (make-leaf 'B 2)
    (make-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(encode (decode sample-message sample-tree) sample-tree)