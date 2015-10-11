#lang racket

(define nil '())

; list functions

(define (list-index p i) ; find p[i]
  (cond ((or (null? p) (< i 0)) (error "List index out of bounds"))
        ((= i 0) (car p))
        (else (list-index (cdr p) (- i 1)))))

(define (length p)
  (define (my-length q curr)
    (if (null? q)
        curr
        (my-length (cdr q) (+ curr 1))))
  (my-length p 0))
          
  

(define (append-r p q) ; append p onto q where p and q are lists
  (if (null? p)
      q
      (cons (car p) (append-r (cdr p) q))))


(define (last-elem p)
  (cond ((null? p) (error "Last element of an empty list does not exist"))
        ((null? (cdr p)) (car p))
        (else (last-elem (cdr p)))))


(define (reverse-list p)
  (define (my-reverse q curr)
    (if (null? q)
        curr
        (my-reverse (cdr q) (cons (car q) curr))))
  (my-reverse p '()))


(define (coin-change val coins) 
  (define first-coin (if (null? coins) nil (car coins)))
  (define except-first-denom (cdr coins))
  (cond ((< val 0) 0)
        ((= val 0) 1)
        ((null? coins) 0) ;now use it or lose it
        (else (+ (coin-change val except-first-denom) (coin-change (- val first-coin) coins)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0 0.5))



(coin-change 100 us-coins)

        
        