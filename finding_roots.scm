#lang racket


;; find roots of a function by half interval method

(define epsilon 1e-8)

(define (avg x y)
  (/ (+ x y) 2.0))

(define (half-interval f a b); find root of f given a,b such that
                             ; one of f(a) or f(b) is positive and the other negative
  (define (good-enough? v)   ;check whether f(v) is close enough to 0
    (< (abs v) epsilon))

  (define (my-half-interval pos-val neg-val)
    (define mid (/ (+ pos-val neg-val) 2.0))
      (cond ((good-enough? (f mid)) mid)
             ((< (f mid) 0) (my-half-interval pos-val mid)) ;negative!
             (else (my-half-interval mid neg-val))))
             
  (cond ((and (> (f a) 0) (< (f b) 0)) (my-half-interval a b))
        ((and (< (f a) 0) (> (f b) 0)) (my-half-interval b a))
        ((good-enough? (f a)) a)
        ((good-enough? (f b)) b)
        (else (error "Values have same sign"))))


(define cube (lambda (x) (* x x x)))
(define f (lambda (x) (cube (- x 0.723412441234))))
(half-interval f 0.5 1.5) ; exact answer is 0.723412441234
(half-interval sin 2.0 4.0) ; exact answer PI
(half-interval (lambda (x) (- (- (cube x) (* 2 x)) 3)) 1 2)

  