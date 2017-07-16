; Exercise 2.40 - generating unique pairs (i, j) where 1 <= j < i <= n
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval x y)
  (if (> x y)
      `()
      (cons x (enumerate-interval (+ x 1) y))))

(define (flatmap proc seq)
  (accumulate append `() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define square (lambda (x) (* x x)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (accumulate (lambda (element rest)
                (if (predicate element)
                    (cons element rest)
                    rest))
              '()
              sequence))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)