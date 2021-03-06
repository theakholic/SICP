#lang racket
(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (fast-mult a b);calculate a*b
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ (fast-mult a (- b 1)) a))))


(define (fast-mult-iter a b)
  (define (my-fast-mult-iter a b c);calculate a*b + c if b is 0 then c
    (cond ((= b 0) c)
          ((even? b) (my-fast-mult-iter (double a) (halve b) c))
          (else (my-fast-mult-iter a (- b 1) (+ c a)))))
  (cond ((< a 0) (- (fast-mult-iter (abs a) b)))
        ((< b 0) (- (fast-mult-iter a (abs b))))
        (else (my-fast-mult-iter a b 0))))