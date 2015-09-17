#lang racket

;Recursive O(log x) function that approximates sine(x)
;Note that if sin(0.1) = 0.09983..
;So for x < 0.1, sin(x) is approximately x [Base case]

(define (sine x)
  (define (cube t)
    (* t t t))
  (define (p x)
    (+ (* 4 p) (* 3 (cube p))))
  (if (< x 0.1)
      x
      (p (sine (/ x 3.0)))));if x > 0.1 then use sin(x) = 3*sin(x/3) - 4(sin(x/3))^3
      