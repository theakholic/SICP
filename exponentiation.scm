#lang racket
;recursive slow iteration
(define (expt-r b n)
  (if (= n 0)
      1
      (* b (expt-r b (- n 1)))))

;iterative slow exponentitaion
(define (expt-i b n)
  (define (my-expt-i b curr n val);this is the iterative process
    (if (= curr n)
      val ;val is the running product of b^k where k = 0,1,...n
      (my-expt-i b (+ curr 1) n (* b val))))
  (cond ((< n 0) (/ 1.0 (expt-i b (- n))));for negative powers
        (else (my-expt-i b 0 n 1))))


(define (fast-exp-r b n)
  (cond
    ((= n 0) 1)
    ((even? n) (fast-exp-r (sqr b) (/ n 2)));b^n = (b^2)^(n/2) if n even
    (else (* b (fast-exp-r b (- n 1)))))); b^n = b*b^(n-1) if n odd

;iterative fast-exponentiation
(define (fast-exp-i b n)
  (define (my-fast-exp-i b curr val);calculates val*b^curr
    (cond ((= curr 0) val)
        ((even? curr) (my-fast-exp-i (sqr b) (/ curr 2) val));square the base! b
        (else (my-fast-exp-i b (- curr 1) (* b val)))))
  (if (< n 0)
      (/ 1.0 (fast-exp-i b (- n)))
      (my-fast-exp-i b n 1)))
      

  
  