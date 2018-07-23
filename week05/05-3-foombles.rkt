;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 05-3-foombles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; foombles-- with and without accumulators

(require rackunit)
(require rackunit/text-ui)

(require "sets.rkt")

;; The information:

;; Foomble ::= Variable 
;;          | (lambda (Variable) Foomble) 
;;          | (Foomble Foomble)
;; Variable = x | y | z | ... | xx | yy | zz | ... 

;; A variable is free if it occurs in a place that is not inside a lambda
;; with the same name. 

;; Information Analysis:

;; The information does not specify the external representation
;; (information) incomplete detail, so we have some freedom in designing
;; the internal representation (data).

;; IMPORTANT: We are NOT writing functions to convert from an external
;; representation to this internal representation (or vice versa).
;; That will come next week.

;; Data Design:

;; We represent foombles using recursive structures.

(define-struct var-foomble (name))
(define-struct lambda-foomble (var body))
(define-struct app-foomble (fn arg))

;; A Foomble is one of
;; (make-var-foomble Symbol)     
;; (make-lambda-foomble Symbol Foomble)  
;; (make-app-foomble Foomble Foomble)
;; interpretation: the cases represent variables, lambdas, and
;; applications, repectively.

;; We could have represented variables using strings instead of
;; symbols, but using symbols makes it a little easier to build
;; examples. 

;; We also could have used a naked symbol rather than a symbol in a
;; struct.  But the representation we chose makes the template
;; clearer.

;; template:
;; foomble-fn : Foomble -> ?
;; (define (foomble-fn f)
;;   (cond
;;     [(var-foomble? f) (... (var-foomble-name f))]
;;     [(lambda-foomble? f) (...
;;                            (lambda-foomble-var f)
;;                            (foomble-fn (lambda-foomble-body f)))]
;;     [(app-foomble? f) (...
;;                         (foomble-fn (app-foomble-fn f))
;;                         (foomble-fn (app-foomble-arg f)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; version without accumulators

;; free-vars : Foomble -> SetOf<Symbol>
;; Produces the set of names that occur free in the given Foomble
;; EXAMPLE:
;; (free-vars (z (lambda (x) (x y)))) = {y, z}
;; strategy: structural decomposition
#;(define (free-vars f) 
   (cond
     [(var-foomble? f) (list (var-foomble-name f))]
     [(lambda-foomble? f) (set-minus
                           (free-vars (lambda-foomble-body f))
                           (lambda-foomble-var f))]
     [(app-foomble? f) (set-union
                         (free-vars (app-foomble-fn f))
                         (free-vars (app-foomble-arg f)))]))

;; remember to uncomment (run-tests free-vars-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; version with accumulator

;; free-vars-acc : Foomble ListOf<Symbol> -> SetOf<Symbol>
;; GIVEN a foomble f that is part of a larger foomble f0
;; WHERE los is the list of symbols that occur in lambdas above f in
;; f0
;; PRODUCES the set of symbols from f that are free in f0.

;; EXAMPLE: 
;; (free-vars-acc (z (lambda (x) (x y))) (list z)) = (list y) 

;; STRATEGY: Struct Decomp on f : Foomble + Accumulator [los]
(define (free-vars-acc f los)
   (cond
     [(var-foomble? f) (if (my-member? (var-foomble-name f) los)
                           empty
                           (list (var-foomble-name f)))
                           ]
     [(lambda-foomble? f) (free-vars-acc (lambda-foomble-body f)
                                         (cons (lambda-foomble-var f)
                                                 los))]
                                           
     [(app-foomble? f) (set-union
                         (free-vars-acc (app-foomble-fn f) los)
                         (free-vars-acc (app-foomble-arg f) los))]))



;; free-vars : Foomble -> SetOf<Symbol>
;; Produces the set of names that occur free in the given Foomble
;; EXAMPLE:
;; (free-vars (z (lambda (x) (x y)))) = {y, z}

;; Strategy: function composition
(define (free-vars f)
  (free-vars-acc f empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; correctness tests

(define-test-suite free-vars-tests

  (check set-equal?
    (free-vars (make-var-foomble 'x))
    (list 'x))

  (check set-equal?
    (free-vars
      (make-lambda-foomble 'x (make-var-foomble 'x)))
    empty)

  (check set-equal?
    (free-vars
      (make-lambda-foomble 'x (make-app-foomble
                           (make-var-foomble 'x)
                           (make-var-foomble 'y))))
    (list 'y))

  (check set-equal?
    (free-vars
      (make-app-foomble
        (make-var-foomble 'z)
        (make-lambda-foomble 'x (make-app-foomble
                           (make-var-foomble 'x)
                           (make-var-foomble 'y)))))
    (list 'z 'y))

  (check set-equal?
    (free-vars
      (make-app-foomble
        (make-var-foomble 'x)
        (make-lambda-foomble 'x (make-app-foomble
                           (make-var-foomble 'x)
                           (make-var-foomble 'y)))))
    (list 'x 'y)))

 (run-tests free-vars-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STRESS TEST

;; tester : (Number -> X) Number -> Number
;; builds an example of size proportional to n, and times free-vars on it.
;; and returns n
(define (tester n)
  (local
    ((define example (build-example n)))
    (time (free-vars example))))


;; build-example : Number -> Foomble
;; returns an example of size proportional to the given number
(define (build-example n)
  (local
    ((define type (remainder n 2)))
    (cond
      [(<= n 10) (make-var-foomble (choose-name n))]
      [(= type 0) (make-lambda-foomble
                    (choose-name n)
                    (build-example (- n 1)))]
      [(= type 1) (make-app-foomble
                    (build-example (- n 1))
                    (build-example (- n 2)))])))


;; Number -> Symbol
(define (choose-name n)
  (cond
    [(<= n 1) 'x]
    [(= n 2) 'y]
    [(= n 3) 'z]
    [(= n 4) 'u]
    [(= n 5) 'v]
    [else (choose-name (remainder n 5))]))

#;(check-equal? 
 (build-example 11)
 (make-app-foomble 
  (build-example 5)
  (build-example 4)))


;; check to see that a recursive structure is really a foomble
;; this is needed to test build-example
(define (really-a-foomble? f)
  (cond
    [(var-foomble? f) (symbol? (var-foomble-name f))]
    [(lambda-foomble? f) (and
                           (symbol? (lambda-foomble-var f))
                           (really-a-foomble? (lambda-foomble-body f)))]
    [(app-foomble? f) (and
                        (really-a-foomble? (app-foomble-fn f))
                        (really-a-foomble? (app-foomble-arg f)))]
    [else false]))

;; n=10 should be a sufficient test for build-example
(check-true (really-a-foomble? (build-example 20)))
 
(define (foomble-size f)
  (cond
    [(var-foomble? f) 1]
    [(lambda-foomble? f) (+ 1 (foomble-size (lambda-foomble-body f)))]
    [(app-foomble? f) (+ 1 
                        (foomble-size (app-foomble-fn f))
                        (foomble-size (app-foomble-arg f)))]))


#;(build-list 40 (lambda (n) (list n (foomble-size (build-example n)))))

;; (foomble-size (build-example 30))
;; (foomble-size (build-example 40))
;; (foomble-size (build-example 45))
;; (foomble-size (build-example 50))

(define-test-suite stress-tests
  (tester 30)
  (tester 40)
  (tester 45)
  (tester 50)
  )

 


#;(run-tests stress-tests)


