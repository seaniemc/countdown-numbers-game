#lang racket
(define start-perm (list -1 -1 -1 -1 1 1 1 1))

 (define x (remove-duplicates (permutations start-perm)))

(define (make-rpn l)
  (append (list 1 1) l (list -1)))

(map make-rpn x)

 ; (define (valid-rpn? e [s 0])
   ; (if(null? e)
      ; (if (= s 1) #t #f)
      ; (if (= (car e) l)
          ; (valid-rpn? (cdr e) (+ 1 s))
           ;()
          ; )))