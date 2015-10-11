#lang racket

(define some-list (cons (list 1 2) (list 3 4)))
;; (1, 2) -> (3 -> 4)


(define next-list (cons some-list some-list))
;; [(1,2) -> (3 -> 4)] -> [(1,2) -> (3 -> 4)]
;; two elements. each element a list of 3 elements
;;

;; (cons x y) stick x at the head of y
;; x:y

;; (cons 2 3) is a PAIR
;; (cons 2 (cons 3 empty)) is a LIST


(define (count-leaves tree)
  (cond [(empty? tree) 0]
        [(not (cons? tree)) 1]
        [else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree)))]))


(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7))))))))

(car (cdr (car (cdr (cdr l1)))))
(car (car l2))
(car
 (car (cdr
       (car (cdr
             (car (cdr
                   (car (cdr
                         (car (cdr
                               (car (cdr l3)))))))))))))


;; Now (list x y)
;; is syntactic sugar for (cons x (cons y empty))
;; so (1 2 3) -> (4 5 6) -> empty
;; (cons x ys)
;; means stick x in front of LIST ys
;; so x : ys

(define y (list 4 5 6))



(define (deep-reverse ys)
  (define (my-deep-reverse xs current)
    (cond [(empty? xs) current]
          [(not (cons? (first xs)))
           (my-deep-reverse (rest xs)
                            (cons (first xs)
                                  current))]
          [else
           (my-deep-reverse (rest xs)
                            (cons (deep-reverse
                                   (first xs))
                                  current))]))
  (my-deep-reverse ys '()))

(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)

;; fringe :: Tree -> List
;; Returns the elements of the tree in left-right order


(define (fringe t)
  (cond [(empty? t) '()]
        [(not (cons? (first t))) (cons (first t) (fringe (rest t)))]
        [else (append (fringe (first t)) (fringe (rest t)))]))

(equal? (fringe (list (list 1 2) (list 3 4 5) (list 8 9 10) 11))
              (list 1 2 3 4 5 8 9 10 11))



;; a mobile contains a left and a right branch
(define-struct mobile (left right))

;; structure can be a number or another mobile
(define-struct branch (length structure))


(define left-branch mobile-left)
(define right-branch mobile-right)

(define (total-weight mobile-var)
  (cond [(and (not (mobile? (left-branch mobile-var)))
              (not (mobile? (right-branch mobile-var))))
         (+ (branch-structure (left-branch mobile-var))
            (branch-structure (right-branch mobile-var)))]
        [(and ((mobile? (left-branch mobile-var)))
              (not (mobile? (right-branch mobile-var))))
         (+ (total-weight (left-branch mobile-var))
            (branch-structure (right-branch mobile-var)))]
        [(and (not (mobile? (left-branch mobile-var)))
              (mobile? (right-branch mobile-var)))
         (+ (branch-structure (left-branch mobile-var))
            (total-weight (right-branch mobile-var)))]
        [(and (mobile? (left-branch mobile-var))
              (mobile? (right-branch mobile-var)))
         (+ (total-weight (left-branch mobile-var))
            (total-weight (right-branch mobile-var)))]))



(define (mobile-balanced mob-var)
  (= (* (total-weight (left-branch mob-var))
        (branch-length (left-branch mob-var)))
     (* (total-weight (right-branch mob-var))
        (branch-length (right-branch mob-var)))))
              
         
  
  

