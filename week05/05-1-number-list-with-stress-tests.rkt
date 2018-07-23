;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 05-1-number-list-with-stress-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

;; comment out all versions of number-list except the one you want,
;; then say (run-stress-tests 0)

;; A NumberedListOf<X> is a ListOf<(list Num X)>
;; INVARIANT: The elements of the list are numbered in order.
;; example: (list (list 1 66) (list 2 88) (list 3 77))

;; number-list : ListOf<X> -> NumberedListOf<X>
;; produce a list like the original, but with the elements numbered.
;; (number-list (list 66 88 77)) = (list (list 1 66) (list 2 88) (list
;; 3 77))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Solution 1:  structural decomposition



#;(define (number-list lst)
  (cond
    [(empty? lst) empty]
    [else (number-list-helper
            (first lst)
            (number-list (rest lst)))]))

;; number-list-helper : X NumberedListOf<X> -> NumberedListOf<X>
;; given x1 and ((1 x2) (2 x3) ...), produce the list
;; ((1 x1) (2 x2) (3 x3) ...)
;; strategy: structural decomposition on numbered-list using abstraction (map)
(define (number-list-helper first-val numbered-list)
  (cons
    (list 1 first-val)
    (map
      ;; (list Number X) -> (list Number X)
      ;; increment the Number
      (lambda (elt)
        (list
          (+ 1 (first elt))
          (second elt)))
      numbered-list)))


(define-test-suite number-list-helper-tests
  (check-equal?
    (number-list-helper 11 (list (list  1 33) (list  2 13) (list 3 42)))
    (list (list 1 11) (list 2 33) (list 3 13) (list  4 42))))

(run-tests number-list-helper-tests)

;; these next tests illustrate functions that build Checks, and also
;; tests that test properties of the answer rather than the whole answer

;; A Check is a (check-equal? ComputedAnswer CorrectAnswer ErrorMessage)

;; ListOf<X> -> Check
;; Given a list lst, check to see that the first elements of
;; (number-list lst) are (1 ... (length lst))
(define (make-test1 lst)
  (check-equal?
   (map first (number-list lst))
   (build-list (length lst) add1)
   (format "first elements of (number-list ~s) should be 1 through ~s"
     lst
     (add1 (length lst)))))

;; ListOf<X> -> Check
;; Given a list lst, check to see that the second elements of
;; (number-list lst) are the same as lst
(define (make-test2 lst)
  (check-equal?
   (map second (number-list lst))
   lst))

(define-test-suite number-list-tests  
  (check-equal? (number-list empty)
                empty
                "(number-list empty) should be empty")
  (check-equal? (number-list '(a b c)) 
                '((1 a) (2 b) (3 c))
                "(number-list '(a b c)) should be ((1 a) (2 b) (3 c))")
  (make-test1 '(11 22 33 44))
  (make-test2 '(11 22 33 44))
  (make-test1 empty)
  (make-test2 empty))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Solution 2: write a more general function that can number a list
;; starting from any number.

;; number-list-from : ListOf<X> Number -> NumberedListOf<X>
;; Produces a list with same elements as lst, but numbered starting at n.
;; EXAMPLE: (number-list-from (list 88 77) 2) 
;;          = (list (list 2 88) (list 3 77))
;; STRATEGY: stuctural decomposition on lst + accumulator [n]

(define (number-list-from lst n)
  (cond
    [(empty? lst) empty]
    [else
      (cons
        (list n (first lst))
        (number-list-from
          (rest lst)
          (+ n 1)))]))

(check-equal? 
  (number-list-from (list 88 77) 2) 
  (list (list 2 88) (list 3 77)))

;; now we can write number-list as a functional composition

;; STRATEGY: functional composition
(define (number-list lst)
  (number-list-from lst 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Solution 3: Version with better purpose statement

;; number-list-from : ListOf<X> Number -> NumberedListOf<X>
;; GIVEN a sublist slst
;; WHERE slst is the n-th sublist of some list lst0
;; PRODUCES a copy of slst numbered according to its position in lst0.
;; STRATEGY: struct decomp [slst : ListOf<X>] 
;;  + accumulator [n]
#;(define (number-sublist slst n)
  (cond
    [(empty? slst) empty]
    [else
      (cons
        (list n (first slst))
        (number-sublist (rest slst) (+ n 1)))]))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tester : (Number -> X) Number -> Number
;; runs fn on the list [0..n-1], prints the time it took 
;; and returns n
(define (tester fn n)
  (local
    ((define ignored  
       (time
        (fn (build-list n (lambda (n) n))))))
    n))

(run-tests number-list-helper-tests)
(run-tests number-list-tests)

(define-test-suite stress-tests
  (tester number-list 10)
  (tester number-list 100)
  (tester number-list 1000)
  (tester number-list 2000)
  (tester number-list 4000)
  (tester number-list 20000))  ; change this to 10000 before saving

(define (run-stress-tests d)
  (run-tests stress-tests))








