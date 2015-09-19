#lang racket

(define (gcd a b)
  (if (= a 0)
      b
      (if (= b 0)
          a
          (gcd b (remainder a b)))))


(define (make-rat n d) ;should make a rational number n/d in lowest terms
  (define g (gcd n d))
  (cond ((< d 0) (make-rat (- n) (- d)))
        ((< n 0) (cons (- (/ (- n) g)) d))
        (else (cons (/ n g) (/ d g)))))

(define (numer r) ; return the numerator of a rational r
  (car r))

(define (denom r) ; return the denominator of a rational r
  (cdr r))


(define (add-rat r1 r2) ;add rationals r1 and r2
  (make-rat (+ (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(define (sub-rat r1 r2)
  (add-rat r1 (make-rat (- (numer r2)) (denom r2))))

(define (mul-rat r1 r2)
  (make-rat (* (numer r1) (numer r2)) (* (denom r1) (denom r2))))

(define (div-rat r1 r2)
  (make-rat (mul-rat r1 (make-rat (denom r2) (numer r2)))))

(define (equal-rat? r1 r2)
  (= (* (numer r1) (denom r2)) (* (numer r2) (denom r1))))

(define (print-rat r)
  (newline)
  (display (numer r))
  (display "/")
  (display (denom r))
  (newline))
           