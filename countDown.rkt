#lang racket
(displayln "Countdown numbers game")

;The list of numbers used to calculate the random target.

(define numbers(list 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100))
; prints out all the numbers the contestant can choose from
(printf "List Of Numbers: ")numbers
;The list of all the operators.
(define ops-list (list '+ '- '/ '*))

;function which produces a random number between 101 and 999
(define myNumber(random 101 999))

; myNumber prints out the random number. 
(printf "Target Number: ") myNumber


;this is a sample list of 6 numbers
(define all-nums (list 100 50 25 10 2 1))
;contains 720 posible unique combinations
 ;(length(remove-duplicates (permutations all-nums)))

; this create a list of all numbers and operators possible
(define main-list (append all-nums ops-list))

;a function which creates all the possible permutations which turns out to be 39916800
;(define noDups (remove-duplicates (permutations main-list)))


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


(calculate-RPN '(20 50 * 6 1 * 5 - 2 3 * +))

;this function gives a true or false if a list is valid rpn
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

(valid-rpn?  (list 100 50 * 20 52 * + ))
(valid-rpn?  (list + 50 * 20 52 * + ))

#|
========The steps I belive which are needed to get the program to work.==========
The program can generate a random taget number.
We can generate all the permutations and combinations of 6 numbers and 5 operators.
> We have a function which evaluates rpn and have a function which will check to see if a list is valid rpn.
> So we need to create a function which passes in the target number and a list of lists containing rpn.
> This function must first check to see if the current list is valid rpn. If it valid rpn then
> the function will keep evaluating the lists and checking each out come against the target to see if
> it is correct. Once it has found the correct answer the program ends.

> To increase the performance of the algorithim, cutting down on the number of redudant permutations
> would be the first step. We would need to remove all permutations which are not valid rpn.
> This can be done through a combination of currying and careasn-product
|#



