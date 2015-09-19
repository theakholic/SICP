#lang racket


(define (smallest-divisor n)
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


(define (prime? n)
  (= (smallest-divisor n) n))


; O(log n) Probabilistic primality testing with Fermat's Little Theorem

(define (exp-mod b e m)
  (define (my-exp-mod b e v m) ;compute v*b^e mod m
    (cond ((= e 0) (remainder v m))
          ((even? e) (remainder (my-exp-mod (remainder (sqr b) m) (/ e 2) v m) m))
          (else (remainder (my-exp-mod b (- e 1) (remainder (* b v) m) m) m))))
  (my-exp-mod b e 1 m))




(define (fermat-prime? p n);
  (define (my-random x) ; generates an integer from 1 to x-1 (x is an integer)
    (+ 1 (random (- x 1))))
  
  (define (my-fermat-prime? p n a); a is the current random number tested
    (cond ((= n 0) true)
          ((= (exp-mod a p p) a) (my-fermat-prime? p (- n 1) (my-random p)))
          (else false)))
  (my-fermat-prime? p n (my-random p)))
      
      

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-seconds) start-time))
      (report-not-prime (- (current-inexact-milliseconds) start-time))))

(define (report-not-prime t)
  (display " *** ")
  (display "NOT PRIME!")
  (display t))

(define (report-prime t)
  (display "   *** ")
  (display "PRIME")
  (display t))



;Search for the next prime after a number
(define (next-prime n); Assume n > 0 of course
  (define (my-search-for-primes curr) ;curr is odd
    (if (prime? curr)
      curr
      (my-search-for-primes (+ curr 2))))
  (if (even? n)
      (my-search-for-primes (+ n 1))
      (my-search-for-primes n)))

(define (divides? k n)
  (= (remainder n k) 0))

(define (test-it? p a);test whether a^p-1 satisfies the miller rabin
 
  (define (find-s n);find s such that 2^s|n but 2^(s+1) does not
    (define (my-find-s x curr)
      (if (divides? 2 x)
          (my-find-s (/ x 2) (+ curr 1))
          curr))
    (my-find-s n 0)) ;RIGHT
  
  (define (get-q n s) (/ n (exp-mod 2 s p))) ;find q such that n/q is of the form 2^k
 
   
  (define (check s q p) ;find a^(s*q) mod p
    (exp-mod a (* s q) p))
  
  (define (do-it q s) ;test whether a^(p-1), a^(p-1)/2,.... a^q form
    (if (= s 1)     ;a sequence of ones or whether -1 is the first non-zero
        true        ;in this sequence (a primitive square root of 1)
        (cond ((= (check s q p) (- p 1)) true)
              ((= (check s q p) 1) (do-it q (/ s 2)))
              (else false))))
  (do-it (get-q (- p 1) (find-s (- p 1))) (/ (- p 1) (get-q (- p 1) (find-s (- p 1))))))
            
    
  
  
  
              


(define (miller-rabin p trials)
  (cond ((= trials 0) true)
        ((test-it? p (+ (random (- p 1)) 1)) (miller-rabin p (- trials 1)))
        (else false)))
  