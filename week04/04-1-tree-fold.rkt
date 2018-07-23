;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 04-1-tree-fold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; tree-fold.rkt

(require rackunit)
(require rackunit/text-ui)

;; Data Definition:

(define-struct leaf (datum))
(define-struct node (lson rson))

;; A Tree is either
;; -- (make-leaf Number)
;; -- (make-node Tree Tree) 

;; no interpretation, this is just a data structure exercise

;; template
;; tree-fn : Tree -> ???
;; (define (tree-fn t)
;;   (cond
;;     [(leaf? t) (... (leaf-datum t))]
;;     [else (...
;;             (tree-fn (node-lson t))
;;             (tree-fn (node-rson t)))]))

;; examples

(define leaf3 (make-leaf 3))
(define leaf4 (make-leaf 4))
(define leaf5 (make-leaf 5))

(define tree1 (make-node 
	       (make-leaf 3)
	       (make-node
		(make-leaf 4)
		(make-leaf 4))))

(define tree2 (make-node 
	       (make-leaf 4)
	       (make-node
		(make-leaf 5)
		(make-leaf 5))))



;; tree-fold  
;;  : (X X -> X) (Number -> X) Tree -> X
;; returns the result of folding the tree with the given functions
;; strategy: 
(define (tree-fold combiner base t)
  (cond
    [(leaf? t) (base (leaf-datum t))]
    [else (combiner
            (tree-fold combiner base 
              (node-lson t))
            (tree-fold combiner base 
              (node-rson t)))]))


;; the strategy for all the following is HOFC
(define (tree-sum t) 
  (tree-fold + (lambda (n) n) t))

(define (tree-min t) 
  (tree-fold min (lambda (n) n) t))

(define (tree-max t) 
  (tree-fold max (lambda (n) n) t))

(define (number-of-nodes t)
  (tree-fold 
   "stub1"
   "stub2"
   t))

(define (increment-all t)
  (tree-fold
   "stub3"
   "stub4"
   t))

(define-test-suite tree-fold-tests
  (check-equal? (tree-sum tree1) 11 "(tree-sum tree1) should be 11")
  (check-equal? (tree-sum tree2) 14 "(tree-sum tree2) should be 14")
  (check-equal? (tree-min tree1) 3  "(tree-min tree1) should be 3")
  (check-equal? (tree-min tree2) 4  "(tree-min tree1) should be 4")
  (check-equal? (number-of-nodes tree1) 5
    "(number-of-nodes tree1) should be 5")
  (check-equal? (number-of-nodes tree2) 5
    "(number-of-nodes tree2) should be 5")
  (check-equal? (increment-all tree1) tree2
    "(increment-all tree1) should be equal to tree2"))

(run-tests tree-fold-tests)

  