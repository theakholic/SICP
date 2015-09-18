#lang racket


;finding fixed_points of fucntions
; when x and f(x) are very close to each other

(define epsilon 1e-8)

(define (fixed-point f starting-guess)
  (define (good-enough? curr next)
    (< (abs (- curr next)) epsilon))
  
  (define (my-fixed-point curr)
    (display curr)
    (newline)
    (define next (f curr))
    (cond ((good-enough? curr next) curr)
          (else (my-fixed-point next))))

  (my-fixed-point starting-guess))



(define (my-sqrt x) ; finding square root of x is equivalent to finding fixed point of
                    ; f: y -> 0.5*(y + x/y) 
  (define (avg x y) (/ (+ x y) 2.0))
  (fixed-point (lambda (y) (avg y (/ x y))) 1.0))


(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)) ;phi = 1 + 1/phi


(define (solve-xpowerx k) ; solve x^x = k
  (cond ((< k 0) (error "error! k cannot be negative"))
        ((= k 1) 1)
        ((< k 1)(error "please choose k > 1"))
        (else (fixed-point (lambda (x) (/ (log k) (log x))) 2.0))))
                


(define (cont-frac-r N D max)
  (define (my-cont-frac curr)
    (cond ((= curr max) (/ (N curr) (D curr)))
          (else (/ (N curr) (+ (D curr) (my-cont-frac (+ curr 1)))))))
  (my-cont-frac 1))


(define (cont-frac N D max) ;iterative version of continued fraction
  
  (define (my-cont-frac curr val) ;val stores cont-frac from curr to n.
   (define (term x)
      (/ (N x) (D x)))
    
    (cond ((= curr 0) val)
          (else (my-cont-frac (- curr 1) (/ (N curr) (+ (D curr) val))))))

  (my-cont-frac max 0.0))

(define (one-by-phi trials) ; estimate 1/phi
  (cont-frac (lambda (x) 1.0)
             (lambda (x) 1.0)
             trials))


(define (estimate-e trials) ;estimate e-2 using continued fractions
  (+ 2 (cont-frac (lambda (x) 1.0)
                  (lambda (n) (cond ((= 1 n) 1)
                                    ((= (remainder n 3) 2) (* 2 (+ (/ (- n 2) 3) 1)))
                                    (else 1)))
                  trials)))


(define (tan-cf x k) ; approximates tan x using continued fractions
  (cont-frac (lambda (y) (cond ((= y 1) x)
                               (else (- (sqr x)))))
             (lambda (n) (- (* 2 n) 1))
             k))


(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

(define (better-sqrt x)
         (fixed-point (average-damp
                       (lambda (y) (/ x y)))
                      1.0))

(define (better-cubert x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))


(define (fast-exp b n) ;calculate b^n
  (define (my-fast-exp base exp val)
    (cond ((= exp 0) val)
          ((even? exp) (my-fast-exp (* base base) (/ exp 2) val))
          (else (my-fast-exp base (- exp 1) (* base val)))))

  (my-fast-exp b n 1.0))

(define (nth-root x n) ; calculate x^(1/n)
  (fixed-point (average-damp (lambda (y) (/ x (fast-exp y (- n 1)))))
               1.0))
                             
    