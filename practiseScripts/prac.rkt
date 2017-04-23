#lang racket
(let fac ([n 10])
    (if (zero? n)
        1
        (* n (fac (sub1 n)))))

(sub1 10) 