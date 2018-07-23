;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-4-books) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

;; Put your solutions here: http://bit.ly/cs5010sp12lect02
;; upload your solution as yourname.rkt
;; best plan is to go to this url, say "create document", and paste in
;; your solution.

;; Lists of books

(define-struct book (author title on-hand price))

;; A Book is a 
;;  (make-book String String Number Number)
;; Interpretation:
;; author is the author’s name
;; title is the title
;; on-hand is the number of copies on hand
;; price is the price in USD

;; book-fn : Book -> ??
;; (define (book-fn b)
;;   (... (book-author b) (book-title b) (book-on-hand b) (book-price b)))

;; A ListOfBooks (LOB) is either
;; -- empty
;; -- (cons Book LOB)

;; lob-fn : LOB -> ??
;; (define (lob-fn lob)
;;   (cond
;;     [(empty? lob) ...]
;;     [else (...
;;             (first lob)
;;             (lob-fn (rest lob)))]))

;; An Inventory is a ListOfBooks.

(define lob1
  (list
    (make-book "Felleisen" "HtDP/1" 20 7)
    (make-book "Wand" "EOPL" 5 50)
    (make-book "Shakespeare" "Hamlet" 0 2)
    (make-book "Shakespeare" "Macbeth" 0 10)))

;; books-out-of-stock : LOB -> LOB
;; returns a list of the books that are out of stock in the given LOB
;; Example:
;; (books-out-of-stock lob1) =

;;  (list
;;    (make-book "Shakespeare" "Hamlet" 0 2)
;;    (make-book "Shakespeare" "Macbeth" 0 10))
;; Strategy: structural decomposition on lob : LOB


(define (books-out-of-stock lob)
  "stub")

(check-equal?
  (books-out-of-stock lob1)
  (list
    (make-book "Shakespeare" "Hamlet" 0 2)
    (make-book "Shakespeare" "Macbeth" 0 10)))

;; book-inventory-value : Book -> Number
;; Returns the value of the copies on hand of the given book
;; (book-inventory-value (make-book "Felleisen" "HtDP/1" 20 7)) = 140
;; strategy: struc decomp on Book
(define (book-inventory-value b)
  (* (book-on-hand b) (book-price b)))

(check-equal?
  (book-inventory-value (make-book "Felleisen" "HtDP/1" 20 7))
  140
  "simple test")

;; inventory-total-value : LOB -> Number
;; Returns the value of all the copies on hand of all the books in the
;; given LOB
;; (inventory-total-value lob1) = 390

(define (inventory-total-value lob)
 "stub")

(check-equal?
  (inventory-total-value lob1)
  390
  "simple test")


