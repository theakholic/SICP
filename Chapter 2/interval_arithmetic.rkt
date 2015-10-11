#lang racket


;interval arithmetic

(define (make-interval low high)
  (if (< high low)
      (make-interval high low)
      (cons low high)))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (add-interval i1 i2)
  (make-interval (+ (lower-bound i1) (lower-bound i2)) (+ (lower-bound i1) (lower-bound i2))))

(define (sub-interval i1 i2)
  (add-interval i1 (make-interval (- (upper-bound i2)) (- (lower-bound i2)))))

(define (mul-interval i1 i2)
  (define p1 (* (lower-bound i1) (lower-bound i2)))
  (define p2 (* (lower-bound i1) (upper-bound i2)))
  (define p3 (* (upper-bound i1) (upper-bound i2)))
  (define p4 (* (upper-bound i1) (lower-bound i2)))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4)))


(define (div-interval i1 i2)
  (cond ((= (width i2) 0) (error "Cannot divide by zero"))
  (else (mul-interval i1
                (make-interval (/ 1.0 (upper-bound i2)) (/ 1.0 (upper-bound i1)))))))



; interval reformatted into center width

(define (make-centered-interval c w)
  (make-interval (- c w) (+ c w)))


(define (center-interval i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width-interval i)
(/ (- (upper-bound i) (lower-bound i)) 2.0))


; Intervals with percentage tolerance
(define (make-center-percent c p) ; c +- p%of c
  (make-centered-interval c (* (/ p 100) (abs c))))

(define center center-interval)
(define (percent-tolerance i)
  (* 100 (abs (/ (width i) (center i)))))



(define i (make-center-percent 10 50)) 
(lower-bound i) 
 (upper-bound i) 
 (center i) 
 (percent-tolerance i) 


;test this with resistors

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))

(define r1 (make-center-percent 100 20))
(define r2 (make-center-percent 200 25))
(par1 r1 r2)
(par2 r1 r2)