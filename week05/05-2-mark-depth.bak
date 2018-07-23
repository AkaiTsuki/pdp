;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 05-2-mark-depth) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; bintree-depth

(require rackunit)
(require rackunit/text-ui)

(define-struct bintree (left data right))

;; A BinTree<X> is either
;; -- empty
;; -- (make-bintree BinTree<X> X BinTree<X>)

;; Template:
;; bintree-fn : BinTree<?> -> ??
;; (define (bintree-fn tree)
;;   (cond
;;     [(empty? tree) ...]
;;     [else (... 
;;            (bintree-fn (bintree-left tree))
;;            (bintree-data tree)
;;            (bintree-fn (bintree-right tree)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; generalized version:

;; mark-depth-from : Bintree<X> Number -> Bintree<Number>
;; Return a bintree like the given one, except that each node is
;; replaced by its depth STARTING FROM d
;; examples: see below
;; strategy: structural decomposition on Bintree<X> [tree] +
;; accumulator [d]

(define (mark-depth-from tree d) 
   (cond
     [(empty? tree) empty]
     [else (make-bintree
            (mark-depth-from (bintree-left tree) (+ d 1))
            d
            (mark-depth-from (bintree-right tree) (+ d 1)))]))

;; mark-depth : Bintree<X> -> Bintree<Number>
;; Return a bintree like the given one, except that each node is
;; replaced by its depth.
;; examples: see below
;; strategy: functional composition

(define (mark-depth tree)
  (mark-depth-from tree 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Version 3: with local and invariant

;;; mark-depth : BinTree<X> -> BinTree<Number>
;;; return a bintree like the original, but with each node containing
;;; its depth in the tree  

;; strategy: struct decomp on BinTree<X> [tree] + acc [d]
#;(define (mark-depth tree0)
  (local
    (; helper : Bintree<X> Number -> BinTree<Number>
     ; INVARIANT: tree is a subtree of tree0 at depth d
     (define (helper tree d)
       (cond
         [(empty? tree) empty]
         [else
          (make-bintree
           (helper (bintree-left tree) (+ d 1))
           d
           (helper (bintree-right tree) (+ d 1)))])))
    (helper tree0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tests

(define-test-suite bintree-tests

  (check-equal?
    (mark-depth
      (make-bintree 
        (make-bintree
          (make-bintree empty "foo" empty)
          "bar"
          empty)
        "baz"
        (make-bintree
          (make-bintree empty "quux" empty)
          "frob"
          empty)))

    (make-bintree 
      (make-bintree
        (make-bintree empty 2 empty)
        1
        empty)
      0
      (make-bintree
        (make-bintree empty 2 empty)
        1
        empty)))

  (check-equal?
    (mark-depth-from 
      (make-bintree 
        (make-bintree
          (make-bintree empty "foo" empty)
          "bar"
          empty)
        "baz"
        (make-bintree
          (make-bintree empty "quux" empty)
          "frob"
          empty))
      10)

    (make-bintree 
      (make-bintree
        (make-bintree empty 12 empty)
        11
        empty)
      10
      (make-bintree
        (make-bintree empty 12 empty)
        11
        empty))))

(run-tests bintree-tests)

