#lang racket
#|;; A solver for the following puzzle:
;; Given 5 integers a, b, c, d, and e,
;; find an expression that combines a, b, c, and d with arithmetic operations (+, -, *, and /) to get e.
 
(require srfi/1)
(require racket/generator)
 
(define ops '(+ - * /))
 
(define (splits l)
  (for/list ([i (in-range 1 (length l))])
    (cons (take l i) (drop l i))))
 
;; produces a sequence (usually lazy) of all possible combinations of `nums'
;; using operators in `ops'.
(define (combine nums)
  (match nums
    ['() (error "impossible")]
    [(list x) (list x)]
    [_ (in-generator
        (for* ([halves (splits nums)]
               [left   (combine (car halves))]
               [right  (combine (cdr halves))]
               [op     ops])
          (yield `(,op ,left ,right))))]))
 
(define (eval-expr e)
  (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) +inf.0)])
    (eval e)))
 
(define (solve n1 n2 n3 n4 target)
  (delete-duplicates
   (for*/list ([p (permutations (list n1 n2 n3 n4))]
               [l (combine p)]
               #:when (= target (eval-expr l)))
     l)))
 (solve 10 10 10 5 200)|#

(require srfi/1)
(require racket/generator)
 
(define (bool<= x y) (implies x y))
 
;; Monadic bind for lists.
(define-syntax-rule (let*/list (clause ...) body ...)
  (for*/list (clause ... [x (let () body ...)]) x))
 
;; lexical ordering on lists.
(define (list<= l1 l2 [<= <=])
  (match* (l1 l2)
    [('() _) #t]
    [(_ '()) #f]
    [((cons x xs) (cons y ys))
     (match* [(<= x y) (<= y x)]
       [(#t #t) (list<= xs ys <=)]
       [(x _) x])]))
 

;;; ok, now we start doing things.
(define ops '(+ - * /))
 
(define (commutative? op)
  (or (eq? '+ op) (eq? '* op)))
 
(define (all-partitions l)
  (match l
    ['() (list (list '() '()))]
    [(cons x xs) (let*/list ([part (all-partitions xs)])
                   (match-define (list as bs) part)
                   (list (list (cons x as) bs)
                         (list as (cons x bs))))]))
 
(define (partitions l)
  (filter (lambda (l) (not (ormap null? l))) (all-partitions l)))
 
(define (commutative-partitions l)
  (for*/list ([part (partitions l)]
              #:when (list<= (first part) (second part)))
    part))
 
;; produces a sequence (usually lazy) of all possible combinations of `nums'
;; using operators in `ops'.
(define (combine nums)
  (match nums
    ['() (error "impossible")] ;; should never get called recursively.
    [(list x) (list x)]
    [_ (in-generator
        (for* ([op ops]
               [halves (if (commutative? op)
                           (commutative-partitions nums)
                           (partitions nums))]
               [left   (combine (first  halves))]
               [right  (combine (second halves))])
          (yield `(,op ,left ,right))))]))
 
(define (eval-expr e)
  (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) +inf.0)])
    (eval e)))
 
(define (solve n1 n2 n3 n4 target)
  (delete-duplicates
   (for*/list ([l (combine (list n1 n2 n3 n4))]
               #:when (= target (eval-expr l)))
     l)))
 
(pretty-print (solve 6 6 5 2 17))