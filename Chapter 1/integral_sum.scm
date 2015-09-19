#lang racket
(define (sum a b term next); sum of f(x) where x varies from a to b
  (if (> a b)
      0
      (+ (term a) (sum (next a) b term next))))

(define (sum-iter a b term next)
  (define (my-sum x y curr)
    (cond ((> x y) curr)
          (else  (my-sum (next x) y (+ curr (term x))))))
  (my-sum a b 0))

(define (my-next x)
  (+ x 1))

(define (cube x)
  (* x x x))


(define (pi-sum a b) ; add 1/a(a+2) + 1/(a+4)(a+6)... which converges to PI/8
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-iter a b pi-term pi-next))


(define (integral f a b dx); approximate integral of f under limits a to b with step size dx
  (define (integral-next x)
    (+ x dx))
  (* dx (sum-iter (+ a (/ dx 2.0)) b f integral-next)))



(define (simpson f a b n) ;approximate integral using simpson's rule
  
  (define h (/ (- b a)  n))

  (define (y k)
    (f (+ a (* h k))))
  
  (define (simp-term k)
    (* (if (even? k) 2 4)
     (y k)))
    
  (* (/ h 3.0) (+ (sum-iter 0 n simp-term my-next)
                  (- (y 0)) (- (if (even? n) 2 4)
                                (y n)))))