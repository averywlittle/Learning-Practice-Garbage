#lang racket/base

(define (list-red items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (list-length items)
  (if (null? items)
      0
      (+ 1 (list-length (cdr items)))))

(define (list-length-iter items)
  (define (length-iter-helper a count)
    (if (null? a)
        count
        (length-iter-helper (cdr a) (+ 1 count))))
  (length-iter-helper items 0))

(define squares (list 1 4 9 16 25))

(list-length squares)
(list-length-iter squares)
(list-ref squares 3)

; Exercise 2.17
; Define a procedure last-pair that returns the list that contains only
; the last element of a given list

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34))

; Exercise 2.18
; Given a list, return a reversed list of the input

(define (reverse items)
  (define (reverse-list items new-items)
    (if (null? items)
        new-items
        (reverse-list (cdr items)(cons (car items) new-items))))
  (reverse-list items '())) ; This implementation of Racket doesn't have
; a way to send an empty list...

(reverse squares)

; Exercise 2.19
; Refactor change-counting program of Section 1.2.2 to take a list of coin definitions
; with which to count change


; Exercise 2.20
; Use dotted-tail notation to write a procedure that takes a list of numbers
; and returns all numbers with the same even-odd parity as the first argument.
; Example: (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)