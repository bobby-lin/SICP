;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |ex 2.50 - 2.51|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 2.50

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (rotate-90 painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0))
  painter))

(define (n-rotate-90 painter n)
  (if (= n 0)
      painter
      (n-rotate-90 (rotate-90 painter) (- n 1))))

(define (flip-horiz-180 painter)
  (n-rotate-90 painter 2))

(define (flip-horiz-270 painter)
  (n-rotate-90 painter 3))

;(paint (flip-horiz-180 einstein))
;(paint (flip-horiz-270 einstein))

; Exercise 2.51

; Implementation analogous to beside procedure


;(paint (below-1 einstein einstein))

; Implementation in terms of beside and suitable rotation operations
(define (below-2 painter1 painter2)
  (n-rotate-90 (beside (rotate-90 painter1) (rotate-90 painter2)) 3))

(paint (below-2 einstein einstein))