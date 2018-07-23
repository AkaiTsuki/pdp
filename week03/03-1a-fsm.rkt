;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 03-1a-fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; finite state machine example

(require rackunit)
(require rackunit/text-ui)

;; ps01 example was a (b | c)* d.

;; let's do a*(b|c)d*  
;; alphabet is all one-character strings

;; A State is one of
;; "state1"    interp: so far we've seen a*
;; "state2"    interp: so far we've seen a*(b|c)
;; "state3"    interp: so far we've seen a*(b|c)d
;; "errorstate" interp: what we have seen is something other than a*,
;;                      a*(b|c), or a*(b|c)d

;; Template:

;; state-fn : State -> ??
;; (define (state-fn s)
;;   (cond
;;     [(string=? s "state1") ...]
;;     [(string=? s "state2") ...]
;;     [(string=? s "state3") ...]
;;     [(string=? s "errorstate") ...]))


;; An Input is a 1String, which is one of
;; "a"
;; "b"
;; "c"
;; "d"
;; any other 1String

;; template
;; input-fn : Input -> ??
;; (define (input-fn inp)
;;   (cond
;;     [(string=? inp "a") ...]
;;     [(string=? inp "b") ...]
;;     [(string=? inp "c") ...]
;;     [(string=? inp "d") ...]
;;     [else ...]))

;; note: a string of length > 1 is not a 1String, and therefore is not
;; a legal input according to the contract.

;; next-state : State Input -> State
;; produces the state that should follow the given state after the
;; given input.
;; examples:
;; (next-state "state1" "a") = "state1"
;; (next-state "state1" "b") = "state2"
;; (next-state "state2" "a") = "errorstate"
;; (next-state "state2" "d") = "state3"
;; see interpretation of State
;; strategy: struct decomp on s : State
(define (next-state s inp)
  (cond
    [(string=? s "state1") (after-state1 inp)]
    [(string=? s "state2") (after-state2 inp)]
    [(string=? s "state3") "errorstate"]
    [(string=? s "errorstate") "errorstate"]))

;; after-state1 : Input -> State
;; returns the state that should follow state1 after the given input
;; examples:
;; (after-state1 "a") = "state1"
;; (after-state1 "b") = "state2"
;; (after-state1 "c") = "state2"
;; (after-state1 "d") = "errorstate"
;; see interp of State for others. 
;; Since we are in state1, we know that up to this point we've seen a*
;; 
;; strategy: struct decomp on inp : Input
(define (after-state1 inp)
  (cond
    [(string=? inp "a") "state1"]   ; we've still only seen a* 
    [(string=? inp "b") "state2"]   ; we've now seen a*b
    [(string=? inp "c") "state2"]   ; we've now seen a*c
    [(string=? inp "d") "errorstate"]  ; we've now seen a*<something else>
    [else "errorstate"]))

;; similarly:
;; after-state2 : Input -> State
;; in state2, we've seen a*(b|c).  Returns a state that describes what
;; we've seen after the given input
(define (after-state2 inp)
  (cond
    [(string=? inp "a") "errorstate"]
    [(string=? inp "b") "errorstate"]
    [(string=? inp "c") "errorstate"]
    [(string=? inp "d") "state3"]        ;; we've now seen a*(b|c)d
    [else "errorstate"]))

;; tests: we have 4 states and 5 inputs, so we have 20 equivalence
;; classes to check

;; we'll eliminate boilerplate by writing a help function and defining
;; some constants. 

(define (fsa-check s inp out)
  (check-equal?
    (next-state s inp)
    out
    (format "(next-state ~a ~a) should be ~a" s inp out)))


;; give names to the typical values:

(define st1 "state1")
(define st2 "state2")
(define st3 "state3")
(define err "errorstate")

(define a "a")
(define b "b")
(define c "c")
(define d "d")
(define ee "e")  ; e is already defined as 2.718...

(define-test-suite fsm-tests

  (fsa-check st1 a st1)
  (fsa-check st1 b st2)
  (fsa-check st1 c st2)
  (fsa-check st1 d err)
  (fsa-check st1 ee err)

  (fsa-check st2 a err)
  (fsa-check st2 b err)
  (fsa-check st2 c err)
  (fsa-check st2 d st3)
  (fsa-check st2 ee err)

  (fsa-check st3 a err)
  (fsa-check st3 b err)
  (fsa-check st3 c err)
  (fsa-check st3 d err)
  (fsa-check st3 ee err)

  (fsa-check err a err)
  (fsa-check err b err)
  (fsa-check err c err)
  (fsa-check err d err)
  (fsa-check err ee err))

(run-tests fsm-tests)

;; When I ran this the first time, I got 5 failures. 3 were errors in
;; the tests and 2 were errors in the code.

;; Moral: sometimes it's your tests that are wrong, but sometimes it's
;; really your code.  Don't just change the tests to match what your
;; code produces.


