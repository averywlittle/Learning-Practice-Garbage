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
; I modified the count leaves algorithm, since that one visits every leaf and that's what is needed
; Only, instead of counting, we append. And because append requires lists, we return the leaves as lists with only 1 item,
; and we return null pointeres as empty lists!
; This works with any list structure (binary, unbalanced, etc.)
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))
  

(define z (list (list 1 2) (list 3 4)))
(fringe z)

; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

; b)
; Return total weight of the mobile
; This currently calculates the length of a mobile
; [ ] total-length needs work
; [ ] total-weight needs work

(define (total-length mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) (branch-length mobile))
        (else (+ (total-length (left-branch mobile))
                      (total-length (right-branch mobile))))))

; c)