; Exercise 2.14 - Demonstrate that Alyssa's program gives different answers for the two ways of computing the parallel resistor

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

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

(define A (make-center-percent 9.00 0.5))
(define B (make-center-percent 9.00 0.4))

(par1 A B)
(par2 A B)

(define X (make-interval 1 1))
(define y (make-interval 1 1))

(par1 X Y)
(par2 X Y)

; A / A, A / B
(div-interval A A)
(div-interval A B)

; Exercise 2.15 - Is par2 a "better" program for parallel resistances than par1?
; In the algebraic expression, we assume that A / A should produce 1. But in our computation, A / A does not produce 1.
; Hence the algebraic transformation from par2 to par1 had produced an error.
; We can avoid this by avoiding repeated number variable.

; Exercise 2.16 - Explain (in general) why equivalent algebraic expressions may lead to different answers.
; This is a dependency problem in interval arithmetic (https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem).
; If a particular interval appears several times in the function params, then the function will lead us to inaccurate answers.
; Hence the reason why the equivalent algebraic expression may lead to different answers is due to the repeated intervals in the calculation params.