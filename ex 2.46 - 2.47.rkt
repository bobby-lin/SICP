; Exercise 2.46

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (list (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (list (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (list (* s (xcor-vect v)) (* s (ycor-vect v))))

(define v1 (make-vect 6 6))
(define v2 (make-vect 4 4))

(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect v1 0.5)

; Exercise 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define frame (make-frame (make-vect 0 0) (make-vect 2 2) (make-vect 4 4)))

(define (origin-vect f)
  (car f))

(define (edge1-vect f)
  (cadr f))

(define (edge2-vect f)
  (cadr (cdr f)))

(origin-vect frame)
(edge1-vect frame)
(edge2-vect frame)

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define frame-cons (make-frame-cons (make-vect 0 0) (make-vect 2 2) (make-vect 4 4)))

(define (origin-vect-cons f)
  (car f))

(define (edge1-vect-cons f)
  (cadr f))

(define (edge2-vect-cons f)
  (cddr f))

(origin-vect-cons frame-cons)
(edge1-vect-cons frame-cons)
(edge2-vect-cons frame-cons)
