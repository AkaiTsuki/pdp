;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 03-4-sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; sets
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")
; (provide my-member? subset? set-equal?)

;; A SetOf<X> is a ListOf<X> with no duplicates

;; note: empty is a SetOf<X>

;; my-member? : X SetOf<X> -> Boolean
;; strategy: HO Function Combination
(define (my-member? x set1)
  (ormap
   (lambda (elt) (equal? x elt))
   set1))
;  (local
;    ((define (test elt) (equal? x elt)))
;    (ormap test set1)))

(define-test-suite my-member-tests
  (check-true (my-member? 3 (list 1 3 5)))
  (check-false (my-member? 4 (list 1 3 5))))

(run-tests my-member-tests)


;; subset? : SetOf<X> SetOf<X> -> Boolean
;; strategy: HO Function Combination
(define (subset? set1 set2)
  (andmap
   (lambda (elt1) (my-member? elt1 set2))
   set1))


;  (local
;    ((define (test elt1) (my-member? elt1 set2)))
;    (andmap test set1)))

(define-test-suite subset-tests
  (check-true (subset? (list 1 3 5) (list 1 3 2 4 5 8)))
  (check-false (subset? (list 1 3 5) (list 1 3 8))))

(run-tests subset-tests)


;; set-equal? : SetOf<X> SetOf<X> -> Boolean
(define (set-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

(check-true (set-equal? (list 1 3 5) (list 3 5 1)))
(check-false (set-equal? (list 1 3 5) (list 1 3 4 5)))
(check-false (set-equal? (list 1 3 5) (list 1 3 5 7)))
(check-false (set-equal? (list 1 3 5 7) (list 1 3 5)))



;; this should have tests


;; set-cons : X SetOf<X> -> SetOf<X>

;; set-union : SetOf<X> SetOf<X> -> SetOf<X>

