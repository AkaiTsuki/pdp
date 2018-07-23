;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-0-0-sexp-mark-level) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(require rackunit)

;; SexpOfNumber is either
;; -- Number
;; -- ListOfSexpOfNumber

;; A ListOfSexpOfNumber
;; -- empty
;; -- (cons SexpOfNumber ListOfSexpOfNumber)

;; sexp-mark-level : SexpofNumber -> SexpOfNumber
;; 

;; sexp-mark-level : ListOfSexpOfNumber Number -> ListOfSexpOfNumber
;; (sexp-mark-level '(a b c (d (e f) g) h)) 1) = (1 1 1 (2 (3 3) 2 1)
;; GIVEN an sexp and a number,
;; WHERE the sexp is at level n in some larger sexp,
;; PRODUCES an sexp like the current one except that each atom is replaced 
;;          by its level number in the larger sexp.
(define (sexp-mark-level sexp n)
  (cond
    [(symbol? sexp) n]
    [else (sexps-mark-level sexp (+ n 1))]))

;; sexps-mark-level : ....
;; GIVEN a list of sexps and a number
;; WHERE all of the sexps are at level n in some larger sexp
;; PRODUCES a list of sexps like the current list, except that each atom
;;          is replaced by its level number in the larger sexp
;; SD + Acc (because it is part of a mutual recursion)
(define (sexps-mark-level sexps n)
  (cond
    [(empty? sexps) empty]
    [else (cons (sexp-mark-level (first sexps) n)
                (sexps-mark-level (rest sexps) n))]))

(check-equal?
 (sexp-mark-level '(a b c (d (e f) g) h) 0)
                  '(1 1 1 (2 (3 3) 2) 1))

;; exercise: write sexps-mark-level using map.
;; exercise: write sexp-mark-level using map to replace sexps-mark-level





