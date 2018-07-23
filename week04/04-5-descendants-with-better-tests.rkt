;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 04-2-descendants-with-better-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; descendants-better-tests.rkt

(require rackunit)
(require rackunit/text-ui)

(require "03-4-sets.rkt")


(define-struct person (name children))
;; A Person is a 
;; (make-person String ListOfPersons)
;; A ListOfPersons (LoP) is one of
;; -- empty
;; -- (cons Person ListOfPersons)

(define alice (make-person "alice" empty))
(define bob (make-person "bob" empty))
(define chuck (make-person "chuck" (list alice bob)))

(define dave (make-person "dave" empty))
(define eddie (make-person "eddie" (list dave)))

(define fred (make-person "fred" (list chuck eddie)))

;;;; Template:
;;;; person-fn : Person -> ??
;;(define (person-fn p)
;;  (... (person-name p) (lop-fn (person-children p))))
;;
;;;; lop-fn : ListOfPersons -> ??
;;(define (lop-fn lop)
;;  (cond
;;    [(empty? lop) ...]
;;    [else (... (person-fn (first p))
;;               (lop-fn (rest p)))]))


;; grandchildren : Person -> LoP
;; produce a list of the grandchildren of the given person.
;; (grandchildren fred) = (list alice bob dave)
(define (grandchildren p)
  (all-children (person-children p)))

;; all-children : LoP -> LoP
;; given a list of persons, produces the list of all their children.
;; (all-children (list fred eddie)) = (list chuck eddie dave)
(define (all-children lop)
  (cond
    [(empty? lop) empty]
    [else (append
           (person-children (first lop))
           (all-children (rest lop)))]))

(define-test-suite grandchildren-tests
  (check set-equal?
   (all-children (list fred eddie)) 
   (list chuck eddie dave))
  ;; same test, but with the answer in a different order
  (check set-equal?
         (all-children (list fred eddie)) 
         (list eddie chuck dave))
  (check set-equal? 
   (grandchildren fred) 
   (list alice bob dave)))



;; descendants : Person -> LoP
;; given a Person, produce the list of his/her descendants
;; all-descendants : LoP -> LoP
;; given a ListOfPersons, produce the list of all their descendants
;; (descendants fred) = (list chuck eddie alice bob dave)
;; (all-descendants (list chuck eddie)) = (list alice bob eddie)
;; strategy: structural decomposition
(define (descendants p)
  (append
   (person-children p)
   (all-descendants (person-children p))))

(define (all-descendants lop)
  (cond
    [(empty? lop) empty]
    [else (append
           (descendants (first lop))
           (all-descendants (rest lop)))]))

(define-test-suite descendants-tests
  (check set-equal? (descendants fred) (list chuck eddie alice bob dave))
  (check set-equal? (all-descendants (list chuck eddie)) (list alice bob dave)))

(run-tests grandchildren-tests)
(run-tests descendants-tests)
    