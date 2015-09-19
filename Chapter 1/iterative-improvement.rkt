#lang racket

(define epsilon 1e-8)

(define (iter-improv good-enough? improve) ;return a procedure that iteratively improves guess
  (define (iterate guess)
    (if (good-enough? guess)
        guess
        (iterate (improve guess))))
  iterate)

(define (my-sqrt x)
  ((iter-improv
   (lambda (y) (< (abs (- x (* y y))) epsilon))
   (lambda (y) (/ (+ y (/ x y)) 2.0))) x))

(define (fixed-point f guess)
  ((iter-improv
   (lambda (x) (< (abs (- (f x) x)) epsilon))
   (lambda (x) (f x))) guess))