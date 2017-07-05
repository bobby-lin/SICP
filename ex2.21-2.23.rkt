; Exercise 2.21 - Comparison between recursive implementation to modify list or using mapping over list

(define square (lambda(x) (* x x)))

(define (square-list-recursive items)
  (if (null? items)
      `nil
      (cons (square (car items)) (square-list-recursive (cdr items)))))

(define (square-list-map items)
  (map (lambda(x) (* x x)) items))

(square-list-recursive (list 1 2 3 4 5))
(square-list-map (list 2 4 6 8 10))

; Exercise 2.22 - Examining the iterative approach

(define (square-list-iter-wrong items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items `nil))

(square-list-iter-wrong (list 2 4 6 8 10))
; The list is reversed because of errors in "cons ..."

(define (square-list-iter-v2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items `nil))

(square-list-iter-v2 (list 2 4 6 8 10))
; nil should not be at the first index of the list

; A possible way to improve the iterative procedure
(define (square-list-iter-mod items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items `nil))

; Exercise 2.23 - Comparing for-each
(define print (lambda (x) (newline) (display x)))
(define is-positive-number (lambda (x) (newline) (display (> x 0))))
;(for-each print (list 1 2 3 4 5))
;(for-each is-positive-number (list 1 2 3 -4 5))

(define (for-each-impl f items)
  (define (iter current-item items)
    (f current-item)
    (if (null? items)
        (display "")
        (iter (car items) (cdr items))))
  (iter (car items) (cdr items)))

(for-each-impl print (list 1 2 3 4 5))
(for-each-impl is-positive-number (list -1 -2 3 -4 5))
