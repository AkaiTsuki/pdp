;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname find-dog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require rackunit)
(require rackunit/text-ui)

;; find-dog : ListOfString -> Boolean
;; returns true if "dog" is in the given list.
;; strategy: structure (ListOfString) on los
(define (find-dog.v0 los)
  (cond
    [(empty? los) false]
    [else (or 
           (string=? (first los) "dog")           
           (find-dog (rest los)))]))

(define-test-suite find-dog-tests
  (check-equal? (find-dog (list "cat" "dog" "weasel")) true)
  (check-equal? (find-dog (list "cat" "elephant" "weasel")) false))

;; find-cat : ListOfString -> Boolean
;; returns true if "cat" is in the given list
;; strategy: structure (ListOfString) on los
(define (find-cat.v0 los)
  (cond
    [(empty? los) false]
    [else (or
           (string=? (first los) "cat")
           (find-cat (rest los)))]))

;; I want to define a test here, but I don't want to run it now
;; because we will be redefining things later.  So I'll use
;; define-test-suite.  This is handy, but you don't need to do it--
;; you can just put your tests at the end.

(define-test-suite find-cat-tests
  (check-equal? (find-cat (list "cat" "dog" "weasel")) true)
  (check-equal? (find-cat (list "elephant" "weasel")) false))

;; find-animal : ListOfString String -> Boolean
;; returns true iff the given string is in the given los.
(define (find-animal los str)
  (cond
    [(empty? los) false]
    [else (or 
           (string=? (first los) str)           
           (find-animal (rest los) str))]))

(define-test-suite find-animal-tests
  (check-equal? (find-animal (list "cat" "elephant" "weasel") "elephant")
    true)
  (check-equal? (find-animal (list "cat" "elephant" "weasel") "beaver")
    false))

(define (find-dog los)
  (find-animal los "dog"))

(define (find-cat los)
  (find-animal los "cat"))

(run-tests
 (test-suite "all tests"
   find-dog-tests
   find-cat-tests
   find-animal-tests))



