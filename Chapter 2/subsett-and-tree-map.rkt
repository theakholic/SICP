#lang racket


;; mapping over trees

(define (tree-map f tree)
  (cond [(empty? tree)
         tree]
        [(cons? (first tree))
         (cons (tree-map f (first tree))
               (tree-map f (rest tree)))]
        [else (cons (f (first tree))
                    (tree-map f (rest tree)))]))


(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(tree-map sqr x)
(tree-map (lambda (x) (* x x x)) x)

(define (member? e xs)
  (if (empty? xs)
      #f
      (or (equal? e (first xs))
          (member? e (rest xs)))))

;; set of all subsets of a list
;; with distinct elements
(define (subsets s)
  (if (empty? s)
      (list '()) ; empty list only subset
      (let ([others (subsets (rest s))])
            (append others
                    (map (lambda (x) (cons (first s) x))
                         others)))))

(subsets (list 1 2 3 4))
