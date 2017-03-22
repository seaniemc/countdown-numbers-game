#lang racket

;function which produces a random number between 101 and 999
(define (rand)  (displayln (random 101 999)))

(define myNumber(rand))
myNumber


;The list of numbers used to calculate the random target.
(define numbers(list 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100))



;this function creates a stack which we will use to push and pop items from the list.
;It was taken from: http://stackoverflow.com/questions/29244677/implementation-of-lifo-list-in-scheme
(define (make-stack)
  (let ((stack '()))
    (lambda (msg . args)
      (cond 
        [(eq? msg 'pop!)  (set! stack (cdr stack))]
        [(eq? msg 'push!) (set! stack (append (reverse args) stack))]
        [(eq? msg 'stack) stack]
        [else "Not valid message!"]))))


