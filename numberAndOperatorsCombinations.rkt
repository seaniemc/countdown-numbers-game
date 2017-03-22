#lang racket

;this function creates all of the possible combinations of the list 
(define all-nums
  (permutations (list 100 50 25 10 2 1)))

(define ops (list '+ '- '* '/))

(define all-ops (cartesian-product ops ops ops ops))

all-ops
;all-nums