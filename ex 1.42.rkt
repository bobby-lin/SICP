; Exercise 1.42 - Composition of functions f(g(x))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(display "Expect: 49, Result = ")
((compose square inc) 6)
(newline)

(display "Expect: 37, Result = ")
((compose inc square) 6)
