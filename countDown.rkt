#lang racket

;function which produces a random number between 101 and 999
(define (rand)  (displayln (random 101 999)))

(define myNumber(rand))
myNumber


;The list of numbers used to calculate the random target.
(define numbers(list 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100))
