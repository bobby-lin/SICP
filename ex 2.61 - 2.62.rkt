; Exercise 2.61
; Assumes that elements are only numbers.

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 4 (list 1 2 3 4 5 6 7 8 9 10))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      `()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))

(intersection-set (list 4 6 8 10 12) (list 1 2 3 4 5 6))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (adjoin-set x original-set)
  (define (iter set)
    (cond ((null? set) (cons x original-set))
          ((= x (car set)) original-set)
          ((< x (car set)) (cons x original-set))
          ((> x (car set)) (iter (cdr set)))))
  (iter original-set))

; Since we did not use element-of-set?, the worst case of growth will be O(n) since we had to iterate through the set.
; But if x is an element in the set, we can just return the original set.
; And if x is smaller than an element in the set, we can return the set and the element. On average, the number of steps could be n/2 faster.

(adjoin-set 6 `())
(adjoin-set 6 (list 4 5 6 7 8 9))
(adjoin-set 100 (list 4 5 6 7 8 9))

; Exercise 2.62
; Assuming set1 and set2 are ordered list.
; Original union-set has a growth of O(n^2) in worst case.
; Implement a union-set that has a growth of O(n)

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))))

(display "\nUnion-set:\n")
(union-set `(1 2 3) `())
(union-set `() `(4 5 6))
(union-set `(1 2 3 4) `(4 5 6)) ; (1 2 3 4 5 6)
(union-set `(1 2 3) `(5 6))
(union-set `(1 2 3 4) `(1 4 5 6)) ; (1 2 3 4 5 6)
(union-set `(1 3 5 7 9 11) `(3 8 11))