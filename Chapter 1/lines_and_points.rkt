#lang racket

;lines in 2d


(define (make-point x y) ;create a point (x,y)
  (cons x y))

(define (x-coord p)
  (car p))

(define (y-coord p)
  (cdr p))


(define (mid-point p1 p2)
  (make-point (/ (+ (x-coord p1) (x-coord p2)) 2.0) (/ (+ (y-coord p1) (y-coord p2)) 2.0)))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-coord p))
  (display ", ")
  (display (y-coord p))
  (display ")"))

(define (make-segment p1 p2) ;make segment from points p1,p2
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (mid-point-segment s)
  (mid-point (start-segment s) (end-segment s)))