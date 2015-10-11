#lang racket



(define (filter predicate sequence)
  (cond [(empty? sequence) '()]
        [(predicate (first sequence))
         (cons (first sequence)
               (filter predicate (rest sequence)))]
        [else (filter predicate (rest sequence))]))
        


(filter odd? (list 1 2 3 4 5 6 7 8 9))

;; accumulate :: (x y -> y) y [x] -> y
;; takes a list and accumulates op from left
;; think of it like (op x1 (accumulate x2 .. xn))
;; if empty then initial
;; otherwise accumulate (x:ys) = op x (accumulate ys)

(define (accumulate op initial sequence) ;op -> operation
  (cond [(empty? sequence) initial]
        [(cons? (first sequence))
          (op (accumulate op initial (first sequence))
              (accumulate op initial (rest sequence)))]
        [else (op (first sequence)
                  (accumulate
                   op
                   initial
                   (rest sequence)))]))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))

(accumulate cons '() (list 1 2 3 4 5))


(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree) ;fringe!
  (cond [(empty? tree) '()]
        [(not (cons? tree)) (cons tree empty)]
        [else (append (enumerate-tree (first tree))
                    (enumerate-tree (rest tree)))]))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
(enumerate-tree (list (list 1 2) (list 3 4 5) (list 8 9 10) 11))


(define (sum-odd-squares tree)
 (accumulate + 0 (map sqr
                  (filter odd? (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4) (list 5 6))))

(define (fib n)
  (if (or (= 0 n) (= 1 n))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (odd-fibs n)
  (accumulate cons '()
              (filter odd? (map fib (enumerate-interval 0 n)))))

(odd-fibs 20)

(define (list-fib-squares n)
  (accumulate
   cons
   '()
   (map sqr (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-squares-odd-integers seq)
  (accumulate
   *
   1
   (map sqr (filter odd? seq))))

(product-squares-odd-integers (list 1 2 3 4 5))


(define (my-map p seq) ;;gottem
  (accumulate
    (lambda (x y)
      (cons (p x) y))
    '()
    seq))

(my-map sqr (list 1 2 3 4 5 6))

(define (my-append seq1 seq2)
  (accumulate
   cons
   seq2
   seq1))

(my-append (list 1 2 3 4 5) (list 6 7 8 9 10 ))

;; accumulate what (+ 1 (my-length rest)
(define (my-length seq)
  (accumulate
   (lambda (x y) (+ y 1))
   0
   seq))


;; evaluate polynomial via accumulation
;; horner's rule
;; polynomial is a list (a_0, ... a_n)
(define (horner-eval x_0 coeffs)
  (accumulate
   (lambda (x y) (+ x (* x_0 y)))
   0
   coeffs))

(horner-eval 2 (list 1 1 1)) ;1*x^2 + 1*x + 1

;; count-leaves
(define (count-leaves t)
  (my-length (enumerate-tree t)))


;; takes a list of sequences of equal length
;; accumulates the corresponding elements
;; using op
;; if [(x11 ... x1n),
;;     ....
;;     (xm1 ... xmn)]
;; then think of it like
;; accumulate (x11, ... xm1)
;; stuck at front of
;; accumulate-n everything except first column

(define (accumulate-n op init seqs)
  (cond [(empty? (first seqs)) '()]
        [else (cons (accumulate op init (map first seqs))
                    (accumulate-n op init (map rest seqs)))]))

(define s (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))


(accumulate-n + 0 s)


(define (dot v w)
  (accumulate
    +
    0
    (accumulate-n
     (lambda (x y) (* x y))
     1
     (list v w))))

;; define matrix*vector

;; (v1 v2 .. vn)* v
;; (vi.dotv)
;; (v1*V)
;; ...
;; vi1 ...     vin (vi*v) is the i'th part
;; ...
;; vm1 ... vmn

(define (matrix-*-vector m v)
  (map (lambda (x) (dot x v)) m))


;; [[a11 ... a1n],
;; ...
;; [ai1 ... ain],
;; ...
;; [am1 ... amn]]

;; a matrix is (v1,..vn)
;; a transpose is the jth element is map get-jth each element


(define (transpose m)
  (if (empty? (first m))
      '()
      (cons (map first m) (transpose (map rest m)))))

(transpose (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)))

(define (transpose-acc m)
  (accumulate-n
   (lambda (x y) (cons x y))
   '()
   m))
(transpose-acc (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)
                 (list 10 11 12)))
   