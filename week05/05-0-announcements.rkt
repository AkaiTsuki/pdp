;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 05-0-announcements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; struct decomp on several structures

;; Book LineItem -> Boolean
;; STRATEGY: function composition
(define (book-match-line-item b li)
  (= (book-isbn-helper b) (line-item-isbn-helper li)))

(define (book-isbn-helper b)
  (book-isbn b))

;; no, no, no!

;; Book LineItem -> Boolean
;; SD on b : Book
(define (book-match-line-item b li)
  (isbn-matches-line-item (book-isbn b) li))

;; Isbn LineItem -> Boolean
;; SD on line-item
(define (isbn-matches-line-item isbn li)
  (= isbn (line-item-isbn li)))

;; 01-5-balls-collide.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct book (title author copies))
;; A Book is a (make-book String String Number)
;; interp.......

;; A LoB is one of
;; --empty
;; --(cons (make-book String String Number)
;;         LoB)

;; Template for LoB:
;; lob-fn : LoB -> ??
;; strategy: SD on lob: LoB
(define (lob-fn lob)
  (cond
    [(empty? lob) ...]
    [else (...
           (book-title (first lob))
           (book-author (first lob))
           (book-copies (first lob))
           (lob-fn (rest lob)))]))


;; occurs-in? : SoS Str -> Boolean
;; loss-occurs-in? : LoSS Str -> Boolean
;; strategy: SD 
(define (occurs-in? sos str)
  (cond
    [(string? sos) (string=? sos str)]
    [else (loss-occurs-in? sos str)]))

(define (loss-occurs-in? loss str)
  (cond
    [(empty? loss) false]
    [else
     (or
      (occurs-in? (first loss) str)
      (loss-occurs-in? (rest loss) str))]))

;; HOFC
(define (loss-occurs-in? loss str)
  (ormap
   (lambda (sos) (occurs-in? sos str))
   loss))

;; SD + HOFC
(define (occurs-in? sos str)
  (cond
    [(string? sos) (string=? sos str)]
    [else (ormap
           (lambda (sos) (occurs-in? sos str))
           loss)]))

(define-struct person (name age children))
;; interp....

;; A Person is a (make-person String Number Persons)
;; A Persons is a ListOf<Person>

;; Person->Person
;; returns a person like p but with the ages of him and his descendants
;; all increased by 1
;; SD
(define (person-increment-ages p)
  (make-person (person-name p)
               (+ 1 (person-age p))
               (persons-increment-ages (persons-children p))))

;; Persons -> Persons
;; HOFC
(define (persons-increment-ages ps)
  (map person-increment-ages ps))

;; SD + HOFC
(define (person-increment-ages p)
  (make-person (person-name p)
               (+ 1 (person-age p))
               (map person-increment-age (persons-children p))))


(map add1 (map sqr lon))
  




