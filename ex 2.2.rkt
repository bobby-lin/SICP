; Exercise 2.2 - Demo Abstraction Barriers
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment x1 y1 x2 y2) (cons (cons x1 y1) (cons x2 y2)))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

; Return a point
(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)
              (/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)))

(print-point (midpoint-segment (make-segment 0.0 0.0 5.0 10.0)))