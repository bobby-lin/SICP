; Exercise 2.24 - Interpreting nested list
(list 1 (list 2 (list 3 4)))

; Box-and-pointer will look something like this:
; 1 , _
;     2, _
;        3, 4

; Tree diagram:
; If we plot the nested list in a tree diagram, we will get a length of 3 with 4 leaves.

; Exercise 2.25 - Pick 7 using car and cdr combination
(define list-1 (list 1 3 (list 5 7) 9))
(define list-2 (list (list 7)))
(define list-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr list-1)))))
(car (car list-2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-3))))))))))))

; Exercise 2.26 - Understanding list operator results
(define x (list 1 2 3))
(define y (list 4 5 6))

; (append x y)
; Result = (1 2 3 4 5 6)
(append x y)

; (cons x y)
; Result = ((1 2 3) 4 5 6)
(cons x y)

; (list x y)
; Result = ((1 2 3) (4 5 6))
(list x y)