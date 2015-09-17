#lang racket
; T_pq : a,b -> bq + a(p+q), bp + aq
; (T_pq)^2 = T_(p^2+q^2)(q^2+2pq)
(define (sum-of-squares x y)
  (+ (sqr x) (sqr y)))

(define (T-pq-a p q a b) ;RIGHT
  (+ (* b q) (* a q) (* a p)))

(define (T-pq-b p q a b) ;RIGHT
  (+ (* b p) (* a q)))

(define (T-pq-square-a p q a b)
  (T-pq-a (sum-of-squares p q) (+ (sqr q) (* 2 p q)) a b))

(define (T-pq-square-b p q a b)
  (T-pq-b (sum-of-squares p q) (+ (sqr q) (* 2 p q)) a b))

(define (Fib-func-a a b);Generate Fib(CURR+1)
  (T-pq-a 0 1 a b))

(define (Fib-func-b a b)
  (T-pq-b 0 1 a b))

(define (fib-square-a a b)
  (T-pq-square-a 0 1 a b))

(define (fib-square-b a b)
  (T-pq-square-b 0 1 a b))


(define (fast-fib n) ;Calculate the nth fibonacci number in O(log n)
  (define (my-fast-fib m a b p q)
    (cond ((= m 0) b)
          ((even? m) (my-fast-fib (/ m 2) a b (sum-of-squares p q) (+ (sqr q) (* 2 p q))))
          (else (my-fast-fib (- m 1) (T-pq-a p q a b) (T-pq-b p q a b) p q))))
  (my-fast-fib n 1 0 0 1));Generate the nth fib


 (define (fib n) 
   (fib-iter 1 0 0 1 n)) 
 (define (fib-iter a b p q count) 
   (cond ((= count 0) b) 
         ((even? count) 
          (fib-iter a 
                    b 
                    (+ (square p) (square q)) 
                    (+ (* 2 p q) (square q)) 
                    (/ count 2))) 
         (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                         (+ (* b p) (* a q)) 
                         p 
                         q 
                         (- count 1))))) 
  
 (define (square x) (* x x)) 
  