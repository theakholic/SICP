#lang racket



(define (product-r a b term next);product of (term a)*(term (next a))*..
  (if (> a b)
      1
      (* (term a) (product-r (next a) b term next))))

(define (product a b term next)
  (define (product-i a b term next curr)
    (if (> a b)
        curr
        (product-i (next a) b term next (* (term a) curr))))
  (product-i a b term next 1))


(define (factorial n)
  (product 1 n (lambda (x) x) (lambda (x) (+ x 1))))

(define (pi-approx n); approximate pi/4 to n terms
  (* 4 (product 1 n (lambda (m)
                 (if (even? m)
                     (/ (+ m 2.0) (+ m 1.0))
                     (/ (+ m 1.0) (+ m 2.0))))
           (lambda (x) (+ x 1)))))
