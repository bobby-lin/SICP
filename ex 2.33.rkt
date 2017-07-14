; Exercise 2.33 - expressing basic list-manipulation as accumulations
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-f p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y)) `() sequence))

(define (square x) (* x x))
(define (add-2 x) (+ x 2))
(map-f square (list 1 2 3 4))
(map-f add-2 (list 1 2 3 4))

(define (append-f seq1 seq2)
  (accumulate cons seq2 seq1))

(append-f (list 1 2 3) (list 4 5 6))

(define (length-f sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length-f (list 1 2 3 4 5))
(length-f (list 2 4 6 8))
(length-f (list 1 2 3 4 5 6 7 8 9 10))
