#lang racket


(define epsilon 1e-8)

(define (fixed-point f starting-guess)
  (define (good-enough? curr next)
    (< (abs (- curr next)) epsilon))
  
  (define (my-fixed-point curr)
    ;(display curr)
    ;(newline)
    (define next (f curr))
    (cond ((good-enough? curr next) curr)
          (else (my-fixed-point next))))

  (my-fixed-point starting-guess))


(define (double f)
  (lambda (x) (f (f x))))

(define inc-2 (double (lambda (x) (+ x 1))))


(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated-appl f n) ; evaluates to a function that takes x->f(f(f(...n times( f(x)..)))
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) ((compose f (repeated-appl f (- n 1))) x))))

(define (avg a b c)
  (/ (+ a b c) 3.0))

(define dx 1e-6)

(define (smoothen f)
  (lambda (x) (avg (f x) (f (+ x dx)) (f (- x dx)))))

(define (n-fold-smoothen-1 f n)
  (if (= 0 n)
      f
      (smoothen (n-fold-smoothen f (- n 1)))))

(define (n-fold-smoothen f n)
  (lambda (x) ((repeated-appl smoothen n) f)))


(define (fast-exp-i b n)
  (define (my-fast-exp-i b curr val);calculates val*b^curr
    (cond ((= curr 0) val)
        ((even? curr) (my-fast-exp-i (sqr b) (/ curr 2) val));square the base! b
        (else (my-fast-exp-i b (- curr 1) (* b val)))))
  (if (< n 0)
      (/ 1.0 (fast-exp-i b (- n)))
      (my-fast-exp-i b n 1)))

(define (avg-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

(define (nth-root x n) ; find fixed point of y -> x/y^n-1 after n-2 average-dampings
  (define average-damp (repeated-appl avg-damp (floor (/ (log n) (log 2)))))
  (if (= n 1)
      x
      (fixed-point (average-damp
                    (lambda (y) (/ x (fast-exp-i y (- n 1)))))
                   1.0)))
               
   
  