#lang racket

;this function creates all of the possible combinations of the list 
(define all-nums (list 100 50 25 10 2 1))

(define ops-list (list '+ '- '* '/))

(define main-list (append all-nums ops-list))

;main-list
;all-ops
;all-nums

(define noDups (remove-duplicates (permutations main-list)))
noDups