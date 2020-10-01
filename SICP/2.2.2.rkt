#lang racket/base

; Exercise 2.24
(list 1 (list 2 (list 3 4)))

; Exercise 2.25
; Produce 7 from each list with combinations of car and cdr
(define a (list 1 3 (list 5 7) 9))
(define b (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr a)))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr b))))))))))))

; Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y) ; Cons will do this weird thing where it more like injects things together
(list x y)

; Exercise 2.27
; Reverse but traverses trees
(define (deep-reverse tree)
  (define (iter t result)
    (cond ((null? t) result)
          ((not (pair? (car t)))
           (iter (cdr t) (cons (car t) result)))
          (else
           (iter (cdr t) (cons (deep-reverse (car t)) result)))))
  (iter tree '()))

(deep-reverse '((1 2) (3 4)))

; Exercise 2.28
; Create a procedure called fringe that takes a tree and returns a list of all the leaves of the tree
(define (fringe tree)
  (define (iter t result)
    (cond ((null? t) result)
          ((not (pair? (car t)))
           (iter (cdr t) (append t result)))
          (else
           (iter (cdr t) result))))
  (iter tree '()))

(define z (list (list 1 2) (list 3 4)))
(fringe z)