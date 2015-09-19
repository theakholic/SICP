#lang racket


(define (make-rect p1 p2) ;opposite corner points
  (cons p1 p2))

(define (c1-rect r)
  (car r))

(define (c2-rect r)
  (cdr r))

(define (length-rect r)
  (abs (- (x-coord (c1-rect r)) (x-coord (c2-rect r)))))

(define (breadth-rect r)
  (abs (- (x-coord (c1-rect r)) (x-coord (c2-rect r)))))

; Area and perimeter don't depend on definition of rect
; they only use the length-rect and breadth-rect procedures
; This abstraction ensures that area and permieter is
; independent of the internal definition of rect.

(define (perimeter-rect r)
  (* 2 (+ (length-rect r) (breadth-rect r))))

(define (area-rect r)
  (* (length-rect r) (breadth-rect r)))




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