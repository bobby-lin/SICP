; Exercise 2.38

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
; Should be 1 / (2 / 3) = 3 / 2

(fold-left / 1 (list 1 2 3))
; Should be (1 / 2) / 3 = 1/6

(fold-right list `() (list 1 2 3))
; Should be (1 (2 (3 ())))

(fold-left list `() (list 1 2 3))
; Should be (((() 1) 2) 3)

; Give a property such that op should guarantee that fold-right and fold-left will produce the same values for any sequence.
(define op +)
(fold-right op 1 (list 1 2 3))
(fold-left op 1 (list 1 2 3))
(fold-right op 9 (list 1 2 3))
(fold-left op 9 (list 1 2 3))

; Exercise 2.39 - reverse a sequence using fold-right and fold-left
(define (reverse-seq sequence)
  (fold-right (lambda (x y) (append y (list x))) `() sequence))

(reverse-seq  (list 1 2 3 4 5))

(define (reverse-seq  sequence)
  (fold-left (lambda (x y) (cons y x)) `() sequence))

(reverse-seq (list 6 7 8 9 10))