; Goal: To practice in designing abstraction barriers.
; Represent a rectangle and compute perimeter and area of the rectangle using constructors and selectors

; Example of an abstraction barrier:
; (High)
; area-rectangle, perimeter-rectangle
; length-rectangle, breadth-rectangle
; distance-line
; x-point, y-point
; (Low)

(define (square x) (* x x))

(define (make-segment x1 y1 x2 y2) (cons (cons x1 y1) (cons x2 y2)))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (distance-line p1 p2)
  (let ((x1 (x-point p1))
    (x2 (x-point p2))
    (y1 (y-point p1))
    (y2 (y-point p2)))
  (sqrt (+ (square (- x1 x2)) (square (- y1 y2))))))
(define (make-rectangle seg1 seg2) (cons seg1 seg2))
(define (length-rectangle rec) (distance-line (car (car rec)) (cdr (car rec))))
(define (breadth-rectangle rec) (distance-line (car (car rec)) (cdr (cdr rec))))
(define (area-rectangle rec) (* (length-rectangle rec) (breadth-rectangle rec)))
(define (perimeter-rectangle rec) (* 2 (+ (length-rectangle rec) (breadth-rectangle rec))))

;Testcase: p1 = (0, 0), p2 = (0 , 3), p3 = (4, 3), p4 = (4, 0)
(define test-rectangle (make-rectangle (make-segment 0 0 0 3) (make-segment 4 3 4 0)))
(area-rectangle test-rectangle) ;12
(perimeter-rectangle test-rectangle) ;14