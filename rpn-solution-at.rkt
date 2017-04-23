#lang racket
(define rpn(myList)
  (let loop ((stack '()))
    (let ((token (read)))
      (cond ((number? token) (loop `(,token ,@stack)))
            ((assq token `((+ ,+) (- ,-) (* ,*) (/ ,/)))
             => (lambda (ass) (loop `(,((cadr ass) (cadr stack) (car stack))
                                      ,@(cddr stack)))))
            (else (car stack))))))

(rpn 1 9 8 7 + / + 8 6 + /)