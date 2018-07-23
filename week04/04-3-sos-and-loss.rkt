;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |sos and loss|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require rackunit)
(require rackunit/text-ui)

;; S-expressions
;; 
;; An Sexp-of-String (SoS) is either
;; -- a String
;; -- a List-of SoS
;; 
;; A List-of SoS (LoSS) is either
;; -- empty
;; -- (cons SoS LoSS)
;; 
;; SoS's:
;; 
;; "abcd"
;; "def"
;; ("abcd" "def")
;; ("abcd" ("abcd" "def"))
;; ("abc" "def" "ghi")
;; (("abc" "def" "ghi")
;;  ("abcd" ("abcd" "def"))
;;  "def"
;;  ("abcd" "def"))
;; 
;; Template:
;; 
;; sos-fn : SoS -> ??
;; loss-fn : LoSS -> ??
;; 
;; (define (sos-fn sos)
;;   (cond
;;     [(string? sos) ...]
;;     [else (... (loss-fn sos))]))
;; 
;; (define (loss-fn loss)
;;   (cond
;;     [(empty? loss) ...]
;;     [else (...   (sos-fn (first loss))
;;                  (loss-fn (rest loss)) ...)]))
;; 

;; occurs-in? : Sos String -> Boolean
;; returns true if the given string occurs somewhere in the given sos.
;; occurs-in-loss? : Loss String -> Boolean
;; returns true if the given string occurs somewhere in the given loss.
;;STRATEGY: structural decomposition [SOS and LOSS]
(define (occurs-in? sos str) ...)


(define (occurs-in-loss? loss str) ...)


(check-equal? (occurs-in? "alice" "alice") true)
(check-equal? (occurs-in? "bob"  "alice") false)

(check-equal? 
 (occurs-in? (list "alice" "bob") "cathy") 
 false)

(check-equal? 
 (occurs-in? 
  (list
   (list "alice" "bob") 
   "carole") 
  "bob") 
 true)

(check-equal? 
 (occurs-in? 
  (list "alice" 
   (list (list "alice" "bob") "dave") 
   "eve")
  "bob")
 true)

;; number-of-strings : Sos -> Number
;; number-of-strings-in-loss : Loss -> Number
;; returns the number of strings in the given sos or loss.

(define (number-of-strings sos) ...)

(define (number-of-strings-in-loss loss) ...)

(check-equal? 
 (number-of-strings
  (list "alice" 
   (list (list "alice" "bob") "dave") 
   "eve"
   "bob"))
 6)

;; subst : Sos String String -> Sos
;; (subst sos old new) returns a sos just like the given one, except that
;; all instances of old are changed to new.
;; subst-in-loss : Loss String String -> Loss
;; similar for loss.
(define (subst sos old new) ...)

(define (subst-in-loss loss old new) ...)
    
;; subst-string : String String String -> String
;; (subst-string str old new) returns new if str = old, otherwise returns str
;; strategy: domain knowledge
(define (subst-string str old new) ...)

;;; FOLLOW THE TEMPLATE FTW!!


(check-equal? 
  (subst
    (list "alice" 
      (list (list "alice" "bob") "dave") 
      "eve"
      "bob")
    "bob"
    "ted")
  (list "alice" 
    (list (list "alice" "ted") "dave") 
    "eve"
    "ted"))
 