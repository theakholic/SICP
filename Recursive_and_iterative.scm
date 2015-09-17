#lang racket

;Here f is a function defined as
; f(n) = n if n < 3
;      = f(n-1) + 2*f(n-2) + 3*f(n-3) otherwise

(define (f n)
  (cond ((< n 3) n)
        (else (+ ((f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))))


;iterative version of f
(define (f-iter n)
  (define (f-iter-my n curr f-n-1 f-n-2 f-n-3);curr is the current position and varies as 3,4,...n
    (define f-n (+ f-n-1 (* 2 f-n-2) (* 3 f-n-3)));calculate f(n) using the definition of f
    (if (= curr n)
      f-n
      (f-iter-my n (+ curr 1) f-n f-n-1 f-n-2)));increment counter, update the old f-value with the new one
  (if (< n 3)
      n
      (f-iter-my n 3 2 1 0)))
      



