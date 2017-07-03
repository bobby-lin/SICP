; Exercise 2.10 - 2.11

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
  (if (and (>= (upper-bound y) 0) (<= (lower-bound y) 0))
      (display "error: denominator should not be 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

(define a (make-interval 2 5))
(define b (make-interval -2 2))
(div-interval a b)
(newline)

; Exercise 2.11 - Modify mul-interval
(define (mul-interval-mod x y)
  (let ((x-lo (lower-bound x))
        (x-hi (upper-bound x))
        (y-lo (lower-bound y))
        (y-hi (upper-bound y)))
     (cond ((and (>= x-lo 0) (>= x-hi 0) (>= y-lo 0) (>= y-hi 0))
          (make-interval (* x-lo y-lo) (* x-hi y-hi))) ; [+, +] * [+, +]
           
         ((and (>= x-lo 0) (>= x-hi 0) (<= y-lo 0) (>= y-hi 0))
          (make-interval (* x-hi y-lo) (* x-hi y-hi))) ; [+, +] * [-, +]
         
         ((and (>= x-lo 0) (>= x-hi 0) (<= y-lo 0) (<= y-hi 0))
          (make-interval (* x-hi y-lo) (* x-lo y-hi))) ; [+, +] * [-, -]
         
         ((and (<= x-lo 0) (>= x-hi 0) (>= y-lo 0) (>= y-hi 0))
          (make-interval (* x-lo y-hi) (* x-hi y-hi))); [-, +] * [+, +]
         
         ((and (<= x-lo 0) (>= x-hi 0) (<= y-lo 0) (>= y-hi 0))
          (make-interval (min (* x-hi y-lo) (* x-lo y-hi))
                         (max (* x-lo y-lo) (* x-hi y-hi)))) ; [-, +] * [-, +]
         
         ((and (<= x-lo 0) (>= x-hi 0) (<= y-lo 0) (<= y-hi 0))
          (make-interval (* x-hi y-lo) (* x-lo y-lo))) ; [-, +] * [-, -]
         
         ((and (<= x-lo 0) (<= x-hi 0) (>= y-lo 0) (>= y-hi 0)) 
          (make-interval (* x-lo y-hi) (* x-hi y-lo))); [-, -] * [+, +]
         
         ((and (<= x-lo 0) (<= x-hi 0) (<= y-lo 0) (>= y-hi 0))
          (make-interval (* x-lo y-hi) (* x-lo y-lo))) ; [-, -] * [-, +]
         
         ((and (<= x-lo 0) (<= x-hi 0) (<= y-lo 0) (<= y-hi 0))
          (make-interval (* x-hi y-hi) (* x-lo y-lo)))))) ; [-, -] * [-, -]

(define a (make-interval 2 4))
(define b (make-interval -2 4))
(define c (make-interval -4 -2))
(mul-interval-mod a a)
(mul-interval a b)
(mul-interval a c)