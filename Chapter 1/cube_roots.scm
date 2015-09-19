;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname cube_roots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (improve-guess guess x);improve guess using newton's method
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3));(2g + x/g^2)/3


(define (good-enough? guess x);guess is good enough when abs(g^3 - x) < 0.01
  (< (abs (- (* guess guess guess) x)) 1e-2)); won't work for very small or large x

(define (cubrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubrt-iter (improve-guess guess x) x)))

(define (my-cubrt x)
  (cubrt-iter 1.0 x))