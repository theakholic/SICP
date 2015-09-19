#lang racket

;create my version of pair of integers

(define (fast-exp b e)
  (define (my-fast-exp b e val) ;compute val*b^e
    (cond ((= e 0) val)
          ((even? e) (my-fast-exp (* b b) (/ e 2) val))
          (else (my-fast-exp b (- e 1) (* b val)))))
  (my-fast-exp b e 1)) 

(define (my-cons x y) ;construct a pair
  (* (fast-exp 2 x) (fast-exp 3 y)))

(define (my-car p) ;first element in a pair
  (define (still-even? p curr)
    (if (even? p)
        (still-even? (/ p 2) (+ curr 1))
        curr))
  (still-even? p 0))

(define (my-cdr p) ;second element in a pair
  (define (more-threes? p curr)
    (if (= (remainder p 3) 0)
        (more-threes? (/ p 3) (+ curr 1))
        curr))
  (more-threes? p 0))

;basically shows the card N >= card all possible tuples (x,y) for x,y in N