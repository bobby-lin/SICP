; Exercise 1.44 - Smoothing a function repeatedly (useful in signal processing)

(define dx 0.00001)
(define pi 3.14)
(define (average x y z) (/ (+ x y z) 3))
(define (function x) (- (* x x) (sin (/ pi 6))))

(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (repeat i g)
    (if (= i 0)
        g
        (repeat (- i 1) (compose f g))))
  (repeat (- n 1) f))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-smooth f n)
  ((repeated smooth n) f))

(display "[smooth, x = 5] Expect: ")
(average (function 5) (function (- 5 dx)) (function (+ 5 dx)))
(display "Result =  ")

(display "[n-smooth, f(x) = x^2 -3, n = 8, x = 5] Expect: 22.000000000533333")
(newline)
(display "Result = ")
((n-smooth (lambda (x) (- (* x x) 3)) 8) 5)