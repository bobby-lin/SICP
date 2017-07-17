; Exercise 2.41 - generating unique triplets and filtering triplets

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

(define (filter predicate sequence)
  (accumulate (lambda (element rest)
                (if (predicate element)
                    (cons element rest)
                    rest))
              '()
              sequence))

(define (unique-triplet n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1))))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (ordered-triplet n s)
  (define (sum-equals? triplet)
    (= s (+ (car triplet) (cadr triplet) (cadr (cdr triplet)))))
  (map append
       (filter sum-equals? (unique-triplet n))))

(ordered-triplet 6 13)
