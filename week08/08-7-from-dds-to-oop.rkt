#lang racket

;; Data Definition
(define-struct foo (first-one left right))
(define-struct bar (lo hi))

;; A Baz is one of
;; -- (make-foo Number Baz Baz)
;; -- (make-bar Number Number)

;; a sample function that follows the baz template:
(define (baz-mymin b n)
  (cond
    [(foo? b) (min
               (foo-first-one b)
               (baz-mymin (foo-left b) n)
               (baz-mymin (foo-right b) n))]
    [(bar? b) (min  
                n (bar-lo b) (bar-hi b))]))
;;;;;;; convert this data definition to an interface and classes.

(define baz<%>
  (interface ()
    ;; list here all the functions that follow the baz template

    ; -> Number
    mymin  

    ;; ...

    ))

;; use class* to define a class that satisfies a given interface
(define foo%
  (class* object% (baz<%>)
    (init-field first-one left right)
    (super-new)
    (define/public (mymin n)
      (min
        first-one
        (send left mymin n)
        (send right mymin n)))))


(define bar%
  (class* object% (baz<%>)
    (init-field lo hi)
    (super-new)
    (define/public (mymin n)
      (min  
        n lo hi))))






    