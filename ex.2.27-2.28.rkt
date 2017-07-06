; Exercise 2.27 - Deep reverse nested lists
(define (deep-reverse items)
  (define (reverse-mod items)
  (define (reverse-iter items)
    (if (null? (cdr items))
        items
        (append (reverse-iter (cdr items)) (list (car items)))))
  (reverse-iter items))
  (define (reverse-iter items)
    (if (null? (cdr items))
        (list (reverse-mod (car items)))
        (append (reverse-iter (cdr items)) (list (reverse-mod (car items))))))
  (reverse-iter items))

(define x (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
;(deep-reverse x)

; Exercise 2.28 - Reading a tree (represented as nested list)
(define x (list (list 1 2) (list 3 4)))
(define y (list (list 5 6) (list 7 8)))

(define (fringe tree)
  (define (tree-iter tree)
    (if (null? (cdr tree))
        (car tree)
        (append (tree-iter (list (car tree))) (tree-iter (cdr tree)))))
  
  (if (not (pair? (car tree)))
      tree
      (fringe (tree-iter tree))))

(fringe x)
(fringe (list x y))
(fringe (list (list x y) (list x y)))
(fringe (list (list (list x y) (list x y)) (list (list x y) (list x y))))
