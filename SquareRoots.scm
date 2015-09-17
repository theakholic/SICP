(define CLOSE-ENOUGH 1e-5)

(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) CLOSE-ENOUGH))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqr-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqr-iter (improve guess x) x)))