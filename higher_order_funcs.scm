#lang racket

(define (accumulate-r start end combiner initial term next) ;recursive accumulate
  (if (> start end)
      initial
      (combiner (term start) (accumulate-r (next start) end combiner initial term next))))

(define (accumulate s e c i t n) ;iterative accumulate
  (define (accumulate-i start end combiner current term next)
    (if (> start end)
        current
        (accumulate-i (next start) end combiner (combiner (term start) current) term next)))
  (accumulate-i s e c i t n))

(define (sum a b term next)
  (accumulate a b (lambda (x y) (+ x y)) 0 term next))


(define (product a b term next)
  (accumulate a b (lambda (x y) (* x y)) 1 term next))

(define (filtered-accumulate-r start end combiner initial term next filter?) ;accumulate only those that are filtered
  (cond ((> start end) initial)
        ((not (filter? start)) (combiner (term start) (filtered-accumulate-r (next start) end combiner initial term next filter)))
        (else (filtered-accumulate-r (next start) end combiner initial term next filter))))

(define (filtered-accumulate start end combiner initial term next filter?)
  (define (filtered-accumulate-i start end combiner current term next filter?)
    (cond ((> start end) current)
          ((filter? start) (filtered-accumulate-i (next start) end combiner (combiner (term start) current) term next filter?))
          (else (filtered-accumulate-i (next start) end combiner current term next filter?))))
  (filtered-accumulate-i start end combiner initial term next filter?))


;sum of squares of prime numbers between a and b



(define (prime? n) ;test whether a number is prime
  (define (smallest-divisor n) ;find the smallest k > 1 such that k|p
    (define (next t)
      (if (= t 2)
          3
          (+ t 2)))
    
    (define (my-smallest-divisor curr n)
      (if (< (sqr curr) n)
          (if (divides? curr n)
              curr
              (my-smallest-divisor (next curr) n))
          (if (= (sqr curr) n)
              curr
              n)))
    
    (define (divides? x n)
      (= (remainder n x) 0))
    
    (my-smallest-divisor 2 n))
  
  (= (smallest-divisor n) n))

(define (sum-of-prime-squares a b) ;sum of squares of primes between a and b
  (filtered-accumulate
   a
   b
   (lambda (x y) (+ x y))
   0
   (lambda (x) (sqr x))
   (lambda (x) (+ x 1))
   prime?))

(define (gcd a b)
  (cond ((= b 0) a)
        ((= a 0) b)
        (else (gcd b (remainder a b)))))

(define (product-relatively-prime n) ;product of all numbers that are relatively prime to n
  (filtered-accumulate
   1
   n
   (lambda (x y) (* x y))
   1
   (lambda (x) x)
   (lambda (x) (+ x 1))
   (lambda (x) (= (gcd n x) 1))))