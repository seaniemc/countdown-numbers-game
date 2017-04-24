#lang racket

;The list of numbers used to calculate the random target.
(define numbers(list 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100))

;The list of all the operators.
(define ops-list (list '+ '- '/ '*))

;function which produces a random number between 101 and 999
(define myNumber(random 101 999))

; myNumber prints out the random number. 
myNumber

;this function creates all of the possible combinations of the list 
(define all-nums (list 100 50 25 10 2 1))
;contains 720 posible combinations
;(length (remove-duplicates (permutations all-nums)))

; this create a list of all numbers and operators possible
(define main-list (append all-nums ops-list))

;a function which creates all the possible permutations which turns out to be 39916800
(define noDups (remove-duplicates (permutations main-list)))


;This function can be used to calculate all instances of valid rpn.
;https://rosettacode.org/wiki/Parsing/RPN_calculator_algorithm#Racket
(define (calculate-RPN expr)
  (for/fold ([stack '()]) ([token expr])
    ;(printf "~a\t -> ~a~N" token stack)
    (match* (token stack)
     [((? number? n) s) (cons n s)]
     [('+ (list x y s ___)) (cons (+ x y) s)]
     [('- (list x y s ___)) (cons (- y x) s)]
     [('* (list x y s ___)) (cons (* x y) s)]
     [('/ (list x y s ___)) (cons (/ y x) s)]
     [('^ (list x y s ___)) (cons (expt y x) s)]
     [(x s) (error "calculate-RPN: Cannot calculate the expression:" 
                   (reverse (cons x s)))])))


;(calculate-RPN '(20 50 * 6 1 * 5 - 2 3 * +))

(define (valid-rpn? e [s 0])
    ; if e is null
    (if(null? e)
       ; if the stack == 1 return true else false
        (if (= s 1)
         #t
         #f)
       (if (number? (car e))

           (valid-rpn? (cdr e) (+ s 1))

      (if(> s 1)
           (valid-rpn? (cdr e) (- s 1))           
           #f))))

(valid-rpn?  (list 100 50 * 6 1 * 5 - 2 3 * +))



