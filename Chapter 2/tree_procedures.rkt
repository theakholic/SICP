#lang racket
; hierarchical data structures

(define nil '())

(define (map func items) ; maps over a list
  (if (null? items)
      nil
      (cons (func (car items))
            (map func (cdr items)))))


(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (map-iter func items) ;iteratively 
  (define (my-reverse items)
    (define (my-rev items rest)
      (if (null? items)
          rest
          (my-rev (cdr items) (cons (car items) rest))))
    (my-rev items '()))
        
  (define (my-map curr rev-items) ;iterative map
    (if (null? rev-items)
        curr
        (my-map (cons (car rev-items) curr) (cdr rev-items))))
  (my-map nil (my-reverse items)))


(define (count-leaves x) ; count the leaves for a tree x
  (if (pair? x)
      (+ (count-leaves (car x))
         (count-leaves (cdr x)))
      (if (null? x)
          0
          1)))



(define (deep-reverse stuff)
  (define (my-deep-reverse items rest)
    (define head (if (null? items) nil (car items)))
    (define tail (if (null? items) nil (cdr items)))
    (cond ((null? items) rest)
          ((pair? head) (my-deep-reverse tail (cons (deep-reverse head) rest)))
          (else (my-deep-reverse tail (cons head rest)))))
  (my-deep-reverse stuff nil))
  

(define (fringe tree) ;return leaves in left-right order
  (cond ((null? tree) nil)
        ((not (pair? tree)) (cons tree nil)) ;if it is a leaf
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))