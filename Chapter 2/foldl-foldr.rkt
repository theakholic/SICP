#lang racket


;; a:b:c:z
;; becomes (op (op (op (op a b) c) z) init)
(define (foldl op init seq)
  (define (iter curr-ans current-seq)
    (if (empty? current-seq)
        curr-ans
        (iter (op curr-ans (first current-seq))
              (rest current-seq))))
  (iter init seq))


(foldl - 0 (list 1 2 3 4 5 6 7 8 9 10))


(define (foldr op init seq)
  (if (empty? seq)
      init
      (op (first seq) (foldr op init (rest seq)))))

(foldr - 0 (list 1 2 3 4 5 6 7 8 9 10))