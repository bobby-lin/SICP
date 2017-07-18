;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |ex 2.44 - 2.45|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Install sicp.plt in racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; Exercise 2.44

(define (right-split painter n)
   (if (= n 0)
       painter
       (let ((smaller (right-split painter (- n 1))))
         (beside painter (below smaller smaller)))))

;(paint (right-split einstein 1))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;(paint (up-split einstein 1))

; Exercise 2.45

(define (split action1 action2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split action1 action2) painter (- n 1))))
          (action1 painter (action2 smaller smaller))))))

(define right-split-new (split beside below))
(paint (right-split-new einstein 1))

(define up-split-new (split below beside))
(paint (up-split-new einstein 1))