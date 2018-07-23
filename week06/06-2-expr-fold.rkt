;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-2-expr-fold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; expr-fold.rkt


(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)

(define-struct sum-exp (exprs))
(define-struct mult-exp (exprs))

;; An Expr is one of
;; -- Number
;; -- (make-sum-exp LOExpr)
;; -- (make-mult-exp LOExpr)
;; Interpretation: a sum-exp represents a sum and a mult-exp
;; represents a multiplication. 

;; A LOExpr is one of
;; -- empty
;; -- (cons Expr LOExpr)

;; (define (expr-fn expr)
;;   (cond
;;     [(number? expr) ...]
;;     [(sum-exp? expr) (... (loexpr-fn (sum-exp-exprs expr)))]
;;     [(mult-exp? expr) (... (loexpr-fn (mult-exp-exprs expr)))]

;; (define (loexpr-fn exprs)
;;   (cond
;;     [(empty? exprs) ...]
;;     [else (...
;;             (first exprs)
;;             (loexpr-fn (rest exprs)))]))

;; value-of : Expr -> Number
;; Strategy: SD on Expr
(define (value-of expr)
  (cond 
    [(number? expr) expr]
    [(sum-exp? expr) (sum
                       (values-of (sum-exp-exprs expr)))]
    [(prod-exp? expr) (product 
                        (values-of (prod-exp-exprs expr)))]))

;; values-of : LOExpr -> ListOf<Number>
;; produce the list of values of 
(define (values-of exprs)
  (map value-of exprs))

;; so we can simplify value-of to
(define (value-of expr)
  (cond 
    [(number? expr) expr]
    [(sum-exp? expr) (sum
                       (map value-of (sum-exp-exprs expr)))]
    [(prod-exp? expr) (product 
                        (map value-of (prod-exp-exprs expr)))]))

;; similarly for operand count:
(define (operand-count expr)
  (cond 
    [(number? expr) 1]
    [(sum-exp? expr) (sum
                       (map operand-count (sum-exp-exprs expr)))]
    [(prod-exp? expr) (sum  ;; !!
                        (map operand-count (prod-exp-exprs expr)))]))

;; and operator count:

(define (operator-count expr)
  (cond 
    [(number? expr) 0]
    [(sum-exp? expr) (+ 1
                       (sum
                         (map operator-count (sum-exp-exprs expr))))]
    [(prod-exp? expr) (+ 1
                        (sum  
                          (map operator-count (prod-exp-exprs expr))))]))

;; So we can abstract these to:

;; expr-fold : (Number -> X) (ListOf<X> -> X) (ListOf<X> -> X) Expr
;;             -> Expr
(define (expr-fold const-fn sum-fn prod-fn expr)
  (cond
    [(number? expr) (const-fn expr)]
    [(sum-exp? expr) (sum-fn
                       (map 
                         (lambda (expr) (expr-fold const-fn sum-fn
                                          prod-fn expr))
                         (sum-exp-exprs expr)))]
    [(mult-exp? expr) (prod-fn
                       (map 
                         (lambda (expr) (expr-fold const-fn sum-fn
                                          prod-fn expr))
                         (mult-exp-exprs expr)))]))

;; Observing that the lambda is repeated, we'll make it a local:
;; Why can't we make it a separate help function?

(define (expr-fold const-fn sum-fn prod-fn expr)
  (local
    ((define (this-fold-fn expr)
       (expr-fold const-fn sum-fn prod-fn expr)
  (cond
    [(number? expr) (const-fn expr)]
    [(sum-exp? expr) (sum-fn
                       (map this-fold-fn
                         (sum-exp-exprs expr)))]
    [(mult-exp? expr) (prod-fn
                       (map this-fold-fn
                         (mult-exp-exprs expr)))]))

(define (sum lst) (foldr + 0 lst))
(define (product lst) (foldr * 1 lst))

(define (value-of expr)
   (expr-fold (lambda (n) n) sum product expr))

ListOf<Number> -> Number

(define (operator-count expr)
  (expr-fold
    (lambda (n) 0)
    (lambda (answers) (+ 1 (sum answers)))
    (lambda (answers) (+ 1 (sum answers)))
    expr))

;;; expr-to-image is somewhat  more involved, but can also be done
;;; fairly easily.

(check-equal?
  (operator-count
    (make-sum-exp
      (list
        (make-mult-exp (list 22 3333 44))
        (make-mult-exp (list 5555 66))
        (make-sum-exp (list 77 88)))))
  4)

(check-equal?
  (value-of
    (make-sum-exp
      (list
        (make-mult-exp (list 2 4))
        (make-mult-exp (list 2 5)))))
  18)

