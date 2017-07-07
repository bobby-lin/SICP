; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; Writing selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define mob1 (make-mobile (make-branch 1 9) (make-branch 3 5)))
(define mob2 (make-mobile (make-branch 2 mob1) (make-branch 6 8)))
(define mob3 (make-mobile (make-branch 2 mob2) (make-branch 6 2)))

(left-branch mob1)
(branch-structure (left-branch mob1)) ; Weight = 9
(branch-length (right-branch mob1)) ; Length = 3

(left-branch mob2)
(right-branch mob2)
(branch-structure (left-branch mob2)) ; Mobile = ((1 9) (3 5))
(branch-length (left-branch mob2)) ; Length = 2

; Implement total-weight procedure
(define (total-weight mobile)
  (define (branch-iter branch)
    (if (pair? (branch-structure branch))
        (+ (branch-iter (left-branch (branch-structure branch))) (branch-iter(right-branch (branch-structure branch)))) ; is mobile
        (branch-structure branch))) ; is weight
  (+ (branch-iter (left-branch mobile)) (branch-iter(right-branch mobile))))

(total-weight mob1) ; Total weight  = 9 + 5 = 14
(total-weight mob2) ; Total weight  = 14 + 8 = 22
(total-weight mob3) ; Total weight  = 14 + 8 = 22 + 2 = 24

; Check if a mobile is balanced
; Conditions:
; Torque applied in top-left branch equals top-right branch -> (Length * Weight on left = Length * Weight on right)
; Each submobiles hanging off the branches are balanced
; Steps:
; Apply is-mobile-balanced? to mobile
; get left-branch
; get right-branch
; if structure of branch is weight, return torque
; else if the mobile is balanced, apply iter to left and right branches of mobile
; compare the torque between left branch and right branch

(define mob-balanced (make-mobile (make-branch 2 3) (make-branch 2 3))) ; Total weight = 6
(define mob-not-balanced (make-mobile (make-branch 1 1) (make-branch 9 9)))
(define mob-balanced-right-submob (make-mobile (make-branch 2 6) (make-branch 2 mob-balanced)))
(define mob-not-balanced-right-submob (make-mobile (make-branch 2 1) (make-branch 2 mob-balanced)))
(define mob-balanced-left-submob (make-mobile (make-branch 2 mob-balanced) (make-branch 2 6)))
(define mob-not-balanced-left-submob (make-mobile (make-branch 2 mob-balanced) (make-branch 2 10)))
(define mob-balanced-both-submob (make-mobile (make-branch 10 mob-balanced) (make-branch 10 mob-balanced)))
(define mob-not-balanced-both-submob (make-mobile (make-branch 2 mob-balanced) (make-branch 2 mob-not-balanced)))

(define (torque length weight) (* length weight))

(define (is-mobile-balanced? mobile)
  (define (is-torque-equals? len-1 wt-1 len-2 wt-2)
    (= (torque len-1 wt-1) (torque len-2 wt-2)))
  (define (mobile-iter mobile)
    (cond ((and (not (pair? (branch-structure (left-branch mobile)))) (not (pair? (branch-structure (right-branch mobile)))))
           (is-torque-equals? (branch-length (left-branch mobile)) (branch-structure (left-branch mobile))
                              (branch-length (right-branch mobile)) (branch-structure (right-branch mobile))))
          
          ((and (not (pair? (branch-structure (left-branch mobile)))) (pair? (branch-structure (right-branch mobile))))
           (and (is-torque-equals? (branch-length (left-branch mobile)) (branch-structure (left-branch mobile))
                                   (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile))))
                (is-mobile-balanced? (branch-structure (right-branch mobile)))))
          
          ((and (pair? (branch-structure (left-branch mobile))) (not (pair? (branch-structure (right-branch mobile)))))
           (and (is-torque-equals? (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile)))
                                   (branch-length (right-branch mobile)) (branch-structure (right-branch mobile)))
                (is-mobile-balanced? (branch-structure (left-branch mobile)))))
          
          (else (and (is-torque-equals? (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile)))
                                        (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile))))
                     (is-mobile-balanced? (branch-structure (left-branch mobile)))
                     (is-mobile-balanced? (branch-structure (right-branch mobile)))))))
  (mobile-iter mobile))

(is-mobile-balanced? mob-balanced) ;true
(is-mobile-balanced? mob-not-balanced) ;false
(is-mobile-balanced? mob-balanced-right-submob) ;true
(is-mobile-balanced? mob-not-balanced-right-submob) ;false
(is-mobile-balanced? mob-balanced-left-submob) ;true
(is-mobile-balanced? mob-not-balanced-left-submob) ;false
(is-mobile-balanced? mob-balanced-both-submob) ;true
(is-mobile-balanced? mob-not-balanced-both-submob) ;false

; Suppose we modify the constructor
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; The branch-structure and right-branch selectors will need to be modified because we can just use cdr to select the values.
; total-weight and is-mobile-balanced will remain unchanged since they are abstract implementations using the selectors.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define mob1 (make-mobile (make-branch 1 9) (make-branch 3 5)))
(branch-structure (left-branch mob1)) ; 9
(branch-length (left-branch mob1)) ; 1
(branch-structure (right-branch mob1)) ; 5
(branch-length (right-branch mob1)) ; 3
(total-weight mob1) ; 14

(define mob-balanced (make-mobile (make-branch 2 3) (make-branch 2 3))) ; Total weight = 6
(define mob-not-balanced (make-mobile (make-branch 1 1) (make-branch 9 9)))
(define mob-balanced-right-submob (make-mobile (make-branch 2 6) (make-branch 2 mob-balanced)))
(define mob-not-balanced-right-submob (make-mobile (make-branch 2 1) (make-branch 2 mob-balanced)))
(define mob-balanced-left-submob (make-mobile (make-branch 2 mob-balanced) (make-branch 2 6)))
(define mob-not-balanced-left-submob (make-mobile (make-branch 2 mob-balanced) (make-branch 2 10)))
(define mob-balanced-both-submob (make-mobile (make-branch 10 mob-balanced) (make-branch 10 mob-balanced)))
(define mob-not-balanced-both-submob (make-mobile (make-branch 2 mob-balanced) (make-branch 2 mob-not-balanced)))

(is-mobile-balanced? mob-balanced) ;true
(is-mobile-balanced? mob-not-balanced) ;false
(is-mobile-balanced? mob-balanced-right-submob) ;true
(is-mobile-balanced? mob-not-balanced-right-submob) ;false
(is-mobile-balanced? mob-balanced-left-submob) ;true
(is-mobile-balanced? mob-not-balanced-left-submob) ;false
(is-mobile-balanced? mob-balanced-both-submob) ;true
(is-mobile-balanced? mob-not-balanced-both-submob) ;false