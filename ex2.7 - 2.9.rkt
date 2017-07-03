; Exercise 2.7 - Implement upper-bound and lower-bound

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

; Exercise 2.8 - Implement sub-interval
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; Exercise 2.9
(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

; Prove that width of the sum (or difference) of two intervals
; = sum (widths of the intervals)
(display "======= Sum ==========")
(newline)
(display "Width of sum of two intervals = ")
(width (add-interval (make-interval 6.12 7.48) (make-interval 4.47 5.18)))
(display "Sum of the widths of two intervals = ")
(+ (width (make-interval 6.12 7.48)) (width (make-interval 4.47 5.18)))

; However, this is not true in multiplication and division
(display "======= Multiplication =========")
(newline)
(display "Width of multiplication of two intervals = ")
(width (mul-interval (make-interval 6.12 7.48) (make-interval 4.47 5.18)))
(display "Multiplication of the widths of two intervals = ")
(* (width (make-interval 6.12 7.48)) (width (make-interval 4.47 5.18)))