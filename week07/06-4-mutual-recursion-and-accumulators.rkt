;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-4-mutual-recursion-and-accumulators) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Mutual Recursion, Accumulators, and Invariants


;; We talked briefly in class about how to use accumulators and
;; invariants in the presence of mutual recursion.  

;; There the mutual recursion was in the context of mutually-recursive
;; data definitions.   Many folks ran into mutual recursion through
;; help functions.  Let's look at an example.

;; foo : LoN -> LoN
;; PURPOSE: Given a list of numbers, returns a list in which each
;; number is replaced by the sum of the number and the length of
;; the sublist starting at that number.
;; EXAMPLE: (10 20 30 40) => (14 23 32 41)
;; STRATEGY: Structural Decomp on lst : LoN
(define (foo lst)
  (cond
    [(empty? lst) empty]
    [else (cons 
            (+ (first lst) (length lst))
            (foo (rest lst)))]))

;; We observe that we keep recomputing (length lst), so we add an
;; accumulator:

;; foo-acc : LoN Number -> LoN
;; GIVEN a LoN lst and a number n
;; WHERE n = (length lst)
;; RETURNS: a list in which each
;; number is replaced by the sum of the number and the length of
;; the sublist starting at that number.
;; EXAMPLE: (10 20 30 40) => (14 23 32 41)
;; STRATEGY: Structural Decomp on lst : LoN + acc [n : Number]
(define (foo-acc lst n)
  (cond
    [(empty? lst) empty]
    [else (cons 
            (+ (first lst) n)
            (foo-acc (rest lst) (- n 1)))]))

;;; observe that if (length lst) = n, 
;;; then (length (rest lst)) = (- n 1).
;;; So if one call to foo-acc satisfies the invariant, so does the
;;; next call.


;; foo : LoN -> LoN
;; PURPOSE: Given a list of numbers, returns a list in which each
;; number is replaced by the sum of the number and the length of
;; the sublist starting at that number.
;; EXAMPLE: (10 20 30 40) => (14 23 32 41)
;; STRATEGY: Function composition
(define (foo lst) (foo-acc lst (length lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now let's imagine a more complicated function.  This function is
;;; like foo, but with all elements whose corresponding length is even
;;; omitted.

;; bar-acc : LoN Number -> LoN
;; GIVEN a LoN lst and a number n
;; WHERE n = (length lst)
;; RETURNS: a list in which 
;; 1. If the length of the sublist starting at a number is even, that
;; element is omitted from the answer, and
;; 2. If the length of the sublist starting at a number is odd, then
;; that number is replaced by the sum of the number and the length of
;; the sublist.
;; EXAMPLE: (10 20 30 40) 4 => (23 41)
;; STRATEGY: Structural Decomp on lst : LoN + acc [n : Number]
(define (bar-acc lst n)
  (cond
    [(empty? lst) empty]
    [else (if (even? n)
              (bar-acc (rest lst) (- n 1))
              (cons 
               (+ (first lst) n)
               (bar-acc (rest lst) (- n 1)))])))

;;; At this point you say: oh, this is too complicated, I need a help
;;; function. So you write:

;; STRATEGY: Structural Decomp on lst : LoN + accumulator [n : Number]
(define (bar-acc lst n)
  (cond
    [(empty? lst) empty]
    [else (bar-helper (first lst) (rest lst) n)]))

;; Is this strategy right?  Yes, this is the right strategy for this
;; function.  You are taking apart the list.  Are you following the
;; template?  Well, maybe.

;; Isn't it just function composition?  Well, maybe.  But it's really
;; structural decomposition with the recursion hidden in the help
;; function.

;; Why do I have to list the strategy as "+ accumulator"?  I don't
;; change n.  Yes, but again, the recursion is there-- it's just
;; hidden in the help function, and the help function DOES change the
;; value of n.

;; Now let's write bar-helper.

;; bar-helper : Number LoN Number -> LoN
;; PURPOSE:  If n is even, returns a list just like rst, except that
;; 1. If the length of the sublist starting at a number is even, that
;; element is omitted from the answer, and
;; 2. If the length of the sublist starting at a number is odd, then
;; that number is replaced by the sum of the number and the length of
;; the sublist.
;; EXAMPLE: (bar-helper 10 (list 20 30 40) 3) => (23 41)
;; STRATEGY: function composition
(define (bar-helper fst rst n)
  (if (even? n)
    (bar-acc rst (- n 1))
    (cons 
      (+ fst n)
      (bar-acc rst (- n 1)))))

;;; Does this function work?  Yes.

;;; Are the Contract, Purpose, and Strategy correct? 






















;;; NONONONONONONONONONONONONONONONONONONO!!!!

;;; According to the Contract, Purpose, and Strategy, it would be
;;; legal to call bar-helper with

(bar-helper 10 (list 20 30 40) 35)

;;; But this would result in a call to bar that looks like

(bar (list 20 30 40) 34)

;;; AND THIS VIOLATES BAR'S INVARIANT.

;; So we haven't captured enough information in bar-helper's contract
;; and purpose statement.  

;; What must we do in order to guarantee that bar-helper's call to bar
;; is legal?

;; We have to guarantee that (length rst) = (- n 1)

;; So a correct function header for bar-helper would be:

;; bar-helper : Number LoN Number -> LoN
;; WHERE (length rst) = (- n 1)
;; PURPOSE:  If n is even, returns a list just like rst, except that
;; 1. If the length of the sublist starting at a number is even, that
;; element is omitted from the answer, and
;; 2. If the length of the sublist starting at a number is odd, then
;; that number is replaced by the sum of the number and the length of
;; the sublist.
;; EXAMPLE: (bar-helper 10 (list 20 30 40) 3) => (23 41)
;; STRATEGY: function composition + acc [n : Number]

;; Why is this + accumulator?   Because there's an invariant.

;; With this additional information, we know that 
;; bar-helper's call to bar-acc satisfies bar-acc's invariant
;; AND
;; bar-acc's call to bar-helper satisfies bar-helper's invariant. [You
;; should go back and look at the code to check]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Is there a better way of doing this?  Yes.  We know that either
;;; way, bar-helper is guaranteed to call  (bar-acc rst (- n 1)) .

;;; So a better way of doing this is to say:

;; STRATEGY: SD + Acc
(define (bar-acc lst n)
  (cond
    [(empty? lst) empty]
    [else (bar-acc-helper2 n
            (first lst)
            (bar-acc (rest lst) (- n 1)))]))

;; and this follows the template, exactly!

;; bar-acc-helper2 : Number Number LoN -> LoN
;; PURPOSE: if n is even, return the lon.  Otherwise, returns a list
;; whose first element is the sum of fst and n, and whose rest is the
;; lon.
;; STRATEGY: function composition
(define (bar-acc-helper2 n fst lon)
  (if (even? n)
    lon
    (cons 
      (+ fst n)
      lon)))

;; bar-acc follows the template!
;; bar-acc-helper2 takes the RESULT of the recursion.
;; bar-acc-helper2 has an easy purpose statement.







