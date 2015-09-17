;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname square_roots_improved) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define (good-enough? guess old-guess x)
  (< (/ (abs (- old-guess guess)) guess) 1e-2))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))


(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 2.0 x))