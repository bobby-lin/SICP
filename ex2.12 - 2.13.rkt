; Exercise 2.12 - Implement make-center-percent and percent
(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define (make-interval x y) (cons x y))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))

(define (percent i)
  (* (round-off (/ (- (upper-bound i) (center i)) (center i)) 2) 100))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(make-center-percent 9.00 5)
(percent (make-center-percent 9.00 5))

; Exercise 2.13 - Show a simple formula to approximate percentage tolerance of product of two intervals (assuming a small percentage tolerance)
; The percentage tolerance will be the sum of the percent of each interval
(display "Using mul-interval, percent = ")
(percent (mul-interval (make-center-percent 1.00 5) (make-center-percent 2.00 5)))
(display "By adding the percent of each interval, percent = ")
(+ (percent (make-center-percent 1.00 5)) (percent (make-center-percent 2.00 5)))

(display "Using mul-interval, percent = ")
(percent (mul-interval (make-center-percent 5.50 2) (make-center-percent 2.00 5)))
(display "By adding the percent of each interval, percent = ")
(+ (percent (make-center-percent 5.50 2)) (percent (make-center-percent 2.00 5)))