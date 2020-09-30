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

(define us-coins (list 25 50 10 1 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; OG algorithm for counting change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

; New algorithm taking in list of coins
(define (cc-list amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc-list amount
                (except-first-denomination
                 coin-values))
            (cc-list (- amount
                   (first-denomination-list
                    coin-values))
                coin-values)))))

; Define first-denomination-list
(define (first-denomination-list coin-list)
  (car coin-list))

; Define except-first-denomination
(define (except-first-denomination coin-list)
  (cdr coin-list))

; Define no-more?
; Where we return false if there is more in the list
; and true if there isn't
(define (no-more? coin-list)
  (if (null? coin-list)
      #t
      #f))

; Calling the function
(cc-list 100 us-coins)

; Does the order of the list coin-calues affect the answer produced by cc?
; The order doesn't seem to matter. I believe this is because we are not
; Iterating through a list of numbers. Rather, we are hitting every element of the list
; while the algorithm responds accordingly.

; Exercise 2.20
; Use dotted-tail notation to write a procedure that takes a list of numbers
; and returns all numbers with the same even-odd parity as the first argument.
; Example: (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)

; Using a simple odd or even function, can be more accurate but I've done
; it before and don't feel like doing it now. Should be fine for these test cases
(define (even? x)
  (remainder x 2)
 )

(define (same-parity . num-list)
  
  ; Get our comparator
  (define comp (even? (car num-list)))
  
  ; Define our recursive function
  (define (same-parity-recur comparator new-list number-list)
    
    ; Test if we're done recurring
    (if (null? number-list)
        new-list
        
        ; Test if we need to add this number or not
        (if (= (even? (car number-list)) comparator)
            (same-parity-recur comparator (cons (car number-list) new-list) (cdr number-list))
            (same-parity-recur comparator new-list (cdr number-list))
            )))
  
  ; Call the function
  (same-parity-recur comp null num-list)
  )

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
(scale-list (list 1 2 3 4 5) 10)

; This general method can be captured within a higher order procedure 'Map'
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (scale-list-map items factor)
  (map (lambda (x) (* x factor))
       items))

; Exercise 2.21
; Define square list in terms of both list and map implementations
(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x) items)))

; Exercise 2.22
; Even if you exchange the cons arguments, the list is still being constructed backwards.
; When it's printed, it will appear reversed.

; Exercise 2.23
; Give an implementation of for-each. What it returns is arbitrary.
(define (for-each proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each proc (cdr items)))))
