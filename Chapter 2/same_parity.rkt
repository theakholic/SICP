#lang racket
(define nil '())

(define (same-parity first . rest) ; takes arbitrary number of elements
  (define (my-filter f p)
    (define head (if (null? p) '() (car p)))
    (define tail (if (null? p) '() (cdr p)))
    (if (null? p)
        '()
        (if (f head)
            (cons head (my-filter f tail))
            (my-filter f tail))))
  (cons first (my-filter (if (even? first)
      even?
      odd?) rest)))