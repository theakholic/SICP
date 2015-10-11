#lang racket


(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n) ; one more than n
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

(define (add-church a b);f(f(...b times...(f(f(... a times ... f(x))))....
  (lambda (f)
    (lambda (x) ((b f) ((a f) x)))))
  