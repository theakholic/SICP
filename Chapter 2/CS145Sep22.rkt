#lang racket

; A  A -> Pair A A
(define (make-pair x y) ; x y t1 t2 
  (cons x y))

(define (first-pair p)
  (car p))

(define (second-pair p)
  (cdr p))

(define (scale p k)
  (make-pair (* (first-pair p) k) (* (second-pair p) k)))


(define (scale-list k p)
  (if (null? p)
      '()
      (cons (* k (car p)) (scale-list k (cdr p)))))



; (A -> Bool) [A] -> [A]
(define (my-filter filter? items) ;remove stuff based on filter? function
  (if  (null? items)
       items
       (if (filter? (car items))
           (my-filter filter? (cdr items))
           (cons (car items) (my-filter filter? (cdr items))))))

(my-filter (lambda (x) (= x 4))
             (cons 4 (cons 3 (cons 4 (cons 4 (cons 3 '()))))))