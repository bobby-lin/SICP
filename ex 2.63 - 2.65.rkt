; Implementing set as a binary tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define test-tree (make-tree 7 (make-tree 3 (make-tree 1 `() `()) (make-tree 5 `() `())) (make-tree 9 `() (make-tree 11 `() `()))))
(entry test-tree)
(left-branch test-tree)
(right-branch test-tree)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(element-of-set? 99 test-tree)

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x `() `()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(adjoin-set 99 test-tree)

; Exercise 2.63


(define tree-1 (make-tree 7 (make-tree 3 (make-tree 1 `() `()) (make-tree 5 `() `())) (make-tree 9 `() (make-tree 11 `() `()))))
(define tree-2 (make-tree 3 (make-tree 1 `() `()) (make-tree 7 (make-tree 5 `() `()) (make-tree 9 `() (make-tree 11 `() `())))))
(define tree-3 (make-tree 5 (make-tree 3 (make-tree 1 `() `()) `()) (make-tree 9 (make-tree 7 `() `()) (make-tree 11 `() `()))))
(define unbalanced-tree (make-tree 1 `() (make-tree 2 `() (make-tree 3 `() (make-tree 4 `() (make-tree 5 `() (make-tree 6 `() `())))))))

(define (tree->list-1 tree)
  (if (null? tree)
      `()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree `()))

; (a) Both procedures produce the same result for every tree.
(tree->list-1 unbalanced-tree)
(tree->list-2 unbalanced-tree)
(tree->list-1 tree-1)
(tree->list-2 tree-1)
(tree->list-1 tree-2)
(tree->list-2 tree-2)
(tree->list-1 tree-3)
(tree->list-2 tree-3)

; (b) Do the two procedures have the same order of growth in the no. of steps required to convert a balanced tree with n elements to a list?
; The order of growth will be O(log n) for tree->list-2.
; Since tree->list-1 uses append procedure, it will occure a cost for going through the left-branch of the tree. So the order of growth would be O(n log n)
;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1) (append (cdr list1) list2))))

; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons `() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                     (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

; (a) Explaing how partial-tree procedure works.

(list->tree (list 1 3 5 7 9 11))
; Drawing the tree:
;     5
;  /    \
; 1      9
;  \    / \
;   3  7  11

; Printing this-entry, left and right in each recursion.
;3   this-entry
;()  left
;()  right
;1   this-entry
;()  left
;(3 () ())  right
;7  this entry
;() left
;() right
;11 this entry
;() left
;() right
;9  this-entry
;(7 () ())  left
;(11 () ())  right
;5  this-entry
;(1 () (3 () ()))  left
;(9 (7 () ()) (11 () ())) right

; partial-tree divides the list into elements on left-tree, elements on right-tree and central node.
; Then in left-tree and right-tree, the process is repeated recursively.

; (b) What is the order of growth in the number of steps required by list-tree to convert a list of n elements
; O(n) - the procedure will visit each of the elements in the set once.

; Exercise 2.65 - Implement O(n) order of growth for union-set and intersection-set using results in ex 2.63 and 2.64.
(define (union-set s1 s2)
  (define (union-set-rec set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((= (car set1) (car set2))
           (cons (car set1) (union-set-rec (cdr set1) (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1) (union-set-rec (cdr set1) set2)))
          ((> (car set1) (car set2))
           (cons (car set2) (union-set-rec set1 (cdr set2))))))
  (list->tree (union-set-rec (tree->list-2 s1) (tree->list-2 s2))))

(define (intersection-set s1 s2)
  (define (intersection-set-rec set1 set2)
    (if (or (null? set1) (null? set2))
        `()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2) (cons x1 (intersection-set-rec (cdr set1) (cdr set2))))
                ((< x1 x2) (intersection-set-rec (cdr set1) set2))
                ((> x1 x2) (intersection-set-rec set1 (cdr set2)))))))
  (list->tree (intersection-set-rec (tree->list-2 s1) (tree->list-2 s2))))

(define set-1 (make-tree 5 (make-tree 1 `() (make-tree 3 `() `())) (make-tree 9 (make-tree 7 `() `()) (make-tree 11 `() `()))))
(define set-2 (make-tree 8 (make-tree 3 `() `()) (make-tree 11 `() `())))

(union-set set-1 set-2)
(intersection-set set-1 set-2)