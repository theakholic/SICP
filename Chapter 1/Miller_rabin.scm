#lang racket

;; Miller Rabin


(define (exp-mod b e m) ; Calculate b^e mod m but return 0 if find a non-trivial square root of unity modulo m
  
  (cond ((= e 0) 1)
        ((even? e) (non-trivial-square (exp-mod b (/ e 2) m) m))
        (else (remainder (* b (exp-mod b (- e 1) m)) m))))

(define (non-trivial-square x m) ; return x*x mod m if x is not a non-trivial square root of 1
  (define t (remainder (* x x) m))
  (cond ((and (not (or (= (remainder x m) 1) (= (remainder x m) (- m 1)))) (= t 1)) 0)
        (else t)))

(define (mil-rab-prime? p tests)
  (define (my-random x) ; random number between 1 and x-1
    (+ 1 (random (- x 1))))
  
  (define (test a p) ; test whether a^p-1 = 1 mod p and that no non-trivial square root of unity is generated
    (= (exp-mod a (- p 1) p) 1))
  
  (cond ((= tests 0) true)
        ((test (my-random p) p) (mil-rab-prime? p (- tests 1)))
        (else false)))


(define (prime? n)
  (mil-rab-prime? n 30))