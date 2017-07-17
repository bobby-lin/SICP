; Exercise 2.42

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

(define (position-row position)
   (car position))

(define (position-col position)
   (cadr position))

(define empty-board `())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

; Check no two queens are in the same row, col and diagonal
(define (safe? col-k positions)
  (let ((current-queen (car positions))
        (rest-of-queens (cdr positions)))
   (let ((row-k (car current-queen)))
     (null?
      (filter (lambda (position)
                (or (= (car position) row-k)
                    (= (abs (- (car position) row-k))
                       (abs (- (cadr position) col-k)))))
              rest-of-queens)))))
  
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)