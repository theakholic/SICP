#lang racket


; Newton's method to find roots of equations

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

(define (fixed-point f start trials) ;find the fixed point of a function
  (define ad
    (average-damp f))
  (define (next x) (f x))
  
  (define (my-fixed-point curr estimate)
    (cond ((> curr trials) estimate)
          (else (my-fixed-point (+ curr 1) (ad estimate)))))
  (my-fixed-point 1 start ))


(define dx 1e-8)

(define (deriv g) ; evaluate to a function that approximates Dg
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define two-x (deriv (lambda (x) (* x x)))) ;should approximate f(x) = 2x

(define (newton-root g guess) ;to find the root of a function g
  (define (newton-change g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-change g)
               guess 100))


(define (newton-sqrt x)
  (newton-root (lambda (y) (- (sqr y) x)) 1.0))


(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (fixed-point-of-transformed-func g transform guess trials)
  (fixed-point (transform g) guess trials))