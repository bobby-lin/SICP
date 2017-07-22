; Exercise 2.66 - implementing lookup procedure

(define (make-record key entry)
  (cons key entry))

(define (key entry)
  (car entry))

(key (make-record 1 123123))

(define (lookup-list-version given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup-list-version given-key (cdr set-of-records)))))

(define test-list-records (list
                           (make-record 1 `(Albert Einstein))
                           (make-record 2 `(Ludwig Beethoven))
                           (make-record 3 `(Claude Shannon))))

; Implement the lookup procedure where the set of records is structured as binary tree (ordered by numerical values of the key).
; Breakdown of lookup procedure:
; (1) If the set is null, return false.
; (2) If the key is found in a record, then return this record.
; (3) Else you iterate to either left or right of the tree (depending on whether the key value is smaller or greater than the key value in the entry node.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree-records (make-tree (make-record 2 `(Ludwig Beethoven))
                                (make-tree (make-record 1 `(Albert Einstein)) `() `())
                                (make-tree (make-record 9 `(Claude Shannon)) (make-tree (make-record 5 `(Charles Darwin)) `() `()) `())))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (cadr set-of-records)))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (caddr set-of-records)))))

(lookup 9 tree-records)
(lookup 5 tree-records)
(lookup 2 tree-records)
(lookup 99 tree-records)
(lookup 1 tree-records)