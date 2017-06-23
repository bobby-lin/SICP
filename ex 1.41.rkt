; Exercise 1.41 - Procedure applying procedures

(define (inc n) (+ n 1))
(define (double f)
  (lambda (x) (f (f x))))

((double inc) 5) ; Ans = 7

(((double double) inc) 5) ; Ans = 9
; Brief explanation of what is going on:
; (double double) = [lambda (x) (double (double x))]
; (double inc) = [lambda (x) (inc (inc x))] = (named as) A
; Evaluate ([lambda (x) (A (A x))] 5) = 9

(((double (double double)) inc) 5) ; Ans = 21