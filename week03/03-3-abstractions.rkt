;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Examples of abstraction/generalization

;;; (abstraction = generalization)

(require rackunit)
(require rackunit/text-ui)

;; A ListOf<X> is either
;; -- empty
;; -- (cons X ListOf<X>)

;; TEMPLATE:
;; lox-fn : ListOf<X> -> ??
;; (define (lox-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... 
;;             (first lst)
;;             (lox-fn (rest lst)))]))
            
;; can use this template for any X: ListOf<Number>, ListOf<Sardine>, ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example 1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ListOf<Number> -> ListOf<Number>
;; strategy: structural decomposition (ListOf<Number>) on lon
(define (add-1-to-each lon) 
  (cond
    [(empty? lon) empty]
    [else (cons (add1 (first lon))
                (add-1-to-each (rest lon)))]))

;; I want to define a test here, but I don't want to run it now
;; because we will be redefining things later.  So I'll use
;; define-test-suite.  This is handy, but you don't need to do it--
;; you can just put your tests at the end.

(define-test-suite add-1-to-each-tests
  (check-equal?
    (add-1-to-each (list 22 33 44))
    (list 23 34 45)))

(define-struct employee (name salary))

;; employee-names : ListOf<Employee> -> ListOf<String>
;; strategy (ListOf<Employee>) on loe
(define (employee-names loe)
  (cond
    [(empty? loe) empty]
    [else (cons (employee-name (first loe))
                (employee-names (rest loe)))]))

(define loe1
  (list (make-employee "Joe" 100)
        (make-employee "Steve" 300)))

(define-test-suite employee-names-tests
  (check-equal? (employee-names loe1) (list "Joe" "Steve")))

;; Generalize these to:

;; apply-to-each : ListOf<X> (X->Y) -> ListOf<Y>
(define (apply-to-each lox fn)
  (cond
    [(empty? lox) empty]
    [else (cons (fn (first lox))
                (apply-to-each (rest lox) fn))]))

;; versions using the generalization

;; ;; strategy: HO Function Composition
;; (define (add-1-to-each lon)
;;   (apply-to-each lon add1))

;; ;; stragegy: HO Function Composition
;; (define (employee-names loe)
;;   (apply-to-each loe employee-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More uses of apply-to-each
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; strategy: HO Function Composition
(define (add-5-to-each lon)
  (local
    ((define (add5 n) (+ n 5)))  
    (apply-to-each lon add5)))

;; Number ListOf<Number> -> ListOf<Number>
;; adds the given number to each element of the list
;; (add-to-each 4 (list 20 30 40)) = (list 24 34 44)
;; strategy: HO Function Composition
(define (add-to-each x lon)
  (local 
    ((define (addx n) (+ n x)))
    (apply-to-each lon addx)))

(define-test-suite add-to-each-tests
  (check-equal?
    (add-to-each 4 (list 20 30 40))
    (list 24 34 44))
  (check-equal?
    (add-5-to-each (list 20 30 40))
    (list 25 35 45)))

;; standard name for this: (map fcn list)
;; map : (X -> Y) ListOf<X> -> ListOf<Y>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example 2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sum : ListOf<Number> -> Number
;; strategy: structural decomposition on ListOf<Number>
(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else (+
           (first lon)
           (sum (rest lon)))]))

;; product : ListOf<Number> -> Number
;; strategy: structural decomposition on ListOf<Number>
(define (product lon)
  (cond
    [(empty? lon) 1]
    [else (*
           (first lon)
           (product (rest lon)))]))

(define-test-suite sum-and-product-tests
  (check-equal? (sum (list 2 3 4)) 9)
  (check-equal? (product (list 2 3 4)) 24))

;;; Generalize these to:

;; reduce: (Number Number -> Number) Number ListOf<Number> -> Number
(define (reduce combiner base lon)
  (cond
    [(empty? lon) base]
    [else (combiner
           (first lon)
           (reduce combiner base (rest lon)))]))

;; ;; strategy: HO Function Composition
;; (define (sum lon)
;;   (reduce + 0 lon))

;; ;; strategy: HO Function Composition
;; (define (product lon)
;;   (reduce * 1 lon))

;;; Can define apply-to-each in terms of reduce:

;; ;; strategy: HO Function Composition
;; (define (apply-to-each lox fn)
;;   (local
;;     ((define (combiner first-guy result-on-the-rest)
;;        (cons (fn first-guy)
;;              result-on-the-rest)))  
;;   (reduce combiner empty lox)))

;; now run the tests

(define-test-suite all-tests
  add-1-to-each-tests
  employee-names-tests
  add-to-each-tests
  sum-and-product-tests)
(run-tests all-tests)
;(run-tests add-1-to-each-tests)
;(run-tests employee-names-tests)
;(run-tests add-to-each-tests)
;(run-tests sum-and-product-tests)
;(run-tests add-1-to-each-tests sum-and-product-tests)
