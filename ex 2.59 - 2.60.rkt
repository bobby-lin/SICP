; Exercise 2.59 - implement union-set

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; Return the set with the element
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(intersection-set `(a b c d e) `(a b c x y z))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(union-set `(1 2 3) `())
(union-set `() `(4 5 6))
(union-set `(1 2 3 4) `(4 5 6)) ; (1 2 3 4 5 6)
(union-set `(1 2 3) `(5 6))

; Exercise 2.60 - allowing duplications in set

(define test-set (list 2 3 2 1 3 2 2))

; Since we allow duplication, the worst case for element-of-set? might take more than n steps.
; Efficiency remains at O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; For adjoin, we simply insert the element into the set because we don't have to check for duplication condition.
; Efficiency is just O(1) now.
(define (adjoin-set x set)
  (cons x set))

; For union, we simply append both set1 and set2 since we don't care about the duplication
; Efficiency is O(1).
(define (union-set set1 set2)
  (append set1 set2))

; For intersection, we still need to scan the entire sets.
; Efficiency remains at O(n).
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))