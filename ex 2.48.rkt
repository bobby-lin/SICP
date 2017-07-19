;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |ex 2.48 - 2.49|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 2.48

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

(define (make-segment start-seg end-seg)
  (list start-seg end-seg))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cadr seg))

(end-segment (make-segment (make-vect 3 4) (make-vect 5 9)))

; Exercise 2.49
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(paint (segments->painter (make-segment (make-vect 3 4) (make-vect 5 9))))