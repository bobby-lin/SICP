; Exercise 2.30

(define test-list (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; Using map and recursion
(define (square-tree tree)
  (define (square x) (* x x))
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (square subtree)))
       tree))

(square-tree test-list)

; Without using higher order procedures
; list-iter: Given a tree (list of items)
; Current item: a number or a list.
; Handle number: (square number) + (next item: number or list)
; Handle list: apply list-iter to list + (next item: number or list)

(define (square-tree tree)
  (define (square x) (* x x))
  (cond ((null? tree) `()) ; Note: In DrRacket, nil is represented as `()
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(square-tree (list 1 2 3))
(square-tree test-list)

; Exercise 2.31 - Abstracting exercise 2.30 procedure to produce tree-map procedure
(define (square x) (* x x))
(define test-list (list 5 (list 10 (list 20 40) 50) (list 60 70)))

(define (square-tree tree) (tree-map square tree))

(define (tree-map f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (f subtree)))
       tree))

(square-tree test-list)

(define (square-tree tree) (tree-map square tree))

(define (tree-map f tree)
  (cond ((null? tree) `())
        ((not (pair? tree)) (f tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(square-tree test-list)

; Exercise 2.32 - Subset of a set as list of lists

(define (subsets s)
  (if (null? s)
      (list `())
      (let ((rest (subsets (cdr s))))
        (display "rest = ")
        (display rest)
        (newline)
        (display "s = ")
        (display s)
        (newline)
        (append rest (map (lambda (rest)
                            (append (list (car s)) rest))
                          rest)))))

(subsets (list 1 2 3))

; First, we look at the output
; rest = (())
; s = (3)
; rest = (()   {append with}     (3)) => by appending (list (car s)) = 3 to each element in rest, we obtain ((3))
; s = (2 3)
; rest = (() (3)   {append with}    (2) (2 3)) => by appending (list (car s)) = 2 to each element in rest, we obtain ((2) (2 3))
; s = (1 2 3)
; Result = (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))