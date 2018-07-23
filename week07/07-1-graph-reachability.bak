;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 07-1-graph-reachability) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "sets.rkt")

;; graph reachability 

;; data definitions using list of edges:

;; A Node is a Symbol
;; A Graph is a ListOf<Edge> with no repeats

(define-struct edge (from to))
;; An Edge is a (make-edge Node Node)

(define graph1
  (list
    (make-edge 'a 'c)
    (make-edge 'b 'a)
    (make-edge 'b 'c)))

(define graph2
  (list
    (make-edge 'a 'b)
    (make-edge 'a 'c)
    (make-edge 'a 'd)
    (make-edge 'b 'c)
    (make-edge 'd 'c)
    (make-edge 'd 'f)
    (make-edge 'c 'e)
    (make-edge 'f 'g)
    (make-edge 'e 'g)))

(define (node=? n1 n2) (symbol=? n1 n2))

;; Node Graph -> Listof<Node>
(define (successors n1 loe)
  (map 
   edge-to  
   (filter
    (lambda (e) (node=? (edge-from e) n1))
    loe)))

(define-test-suite successors-tests
  
  (check set-equal?
    (successors 'a (list (make-edge 'a 'b) (make-edge 'a 'c)))
    (list 'b 'c))
 
  (check set-equal?
    (successors 'a graph2)
    '(b c d)))

(run-tests successors-tests)

(define-test-suite path-tests
  (check-equal?
   (path? graph2 'a 'g) 
   true
   "there should be a path from a to g in graph2")

  (check-equal?
   (path? graph2 'b 'd)
   false
   "should find no path from b to d")
  
  (check-equal?
   (path? graph2 'd 'g)
   true
   "should find a path from d to g")
  
  (check-equal?
   (path? graph2 'e 'd)
   false
   "should find no path from e to d")
  
  (check-equal? (path? graph1 'a 'b) false)

  (check-equal? (path? graph1 'b 'c) true))

;; path? : Graph Node Node -> Boolean
;; determines whether there is a path in g from src to tgt
;; strategy: general recursion
;; termination argument: can fail to terminate if g has cycles
;; algorithm: if tgt is the same as src, return true,
;; otherwise search from the successors of src.
;; STRATEGY: GENERAL RECURSION
#;(define (path? g src tgt) 
  (cond
    [(node=? src tgt) true]
    [else
     (ormap
      (lambda (n) (path? g n tgt))
      (successors src g))]))
 

   
;; just for convenience, let's give a name to the lambda expression:

#;(define (path?.v1 g src0 tgt)
  (local
    (;; Node -> Boolean
     (define (helper node) (path? g node tgt)))
    (cond
      [... ...])))

;; Version 2 : add an accumulator srcs to keep track of the nodes we've
;; promised to visit.  Make sure there are no duplicates


;; path-from-any? : Graph Node ListOf<Node> -> Boolean
;; WHERE there are no duplicates in srcs
;; Is there a path in g from any of the srcs to tgt?
(define (path-from-any? g tgt srcs)
  (cond
    [(empty? srcs) false]
    [(node=? (first srcs) tgt) true]
    [else (path-from-any? g tgt
                          (append
                           (filter
                            (lambda (n) (not (member? n srcs)))
                            (successors (first srcs) g))
                           (rest srcs)))]))
           


;; IS THIS BREADTH-FIRST OR DEPTH-FIRST?


  ;; (cond
  ;;   [(empty? srcs) false]
  ;;   [(node=? (first srcs) tgt) true]
  ;;   [else (path-from-any? 
  ;;          g tgt
  ;;          (set-union 
  ;;           (successors (first srcs) g)
  ;;           (rest srcs)))]))

#;(define (path? g src tgt)
  (path-from-any? g tgt (list src)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Well, .v2 didn't prevent a node from re-entering srcs.
;; So, let's keep track of all the nodes that have ever been on srcs,
;; and don't search them again
;;;;;;;;;;;;;;;;;;;;;;;;

;; path-from-any-no-repeats? : Graph Node ListOf<Node> ListOf<Node> -> Boolean
;; Is there a path from g from any of the srcs to the tgt, not passing
;; through seen ?
;; WHERE: srcs and seen are disjoint
(define (path-from-any-no-repeats? g tgt srcs seen)
  (cond
    [(empty? srcs) false]
    [(node=? (first srcs) tgt) true]
    [else (path-from-any-no-repeats? g tgt
            (append
             (filter
              (lambda (n) 
                (and (not (member? n srcs))
                     (not (member? n seen))))
              (successors (first srcs) g))
             (rest srcs))
            (cons (first srcs) seen))]))




  ;; (cond
  ;;   [(empty? srcs) false]
  ;;   [(node=? (first srcs) tgt) true]
  ;;   [else (path-from-any-no-repeats? g tgt
  ;;           (append               
  ;;             (rest srcs)           
  ;;             (filter
  ;;               (lambda (n) (and (not (member? n srcs))
  ;;                             (not (member? n seen))))
  ;;               (successors (first srcs) g)))
  ;;           (cons (first srcs) seen))]))

(define (path? g src tgt)
  (path-from-any-no-repeats? g tgt (list src) empty))
  
;; does this code depend on the representation of the graph?

(run-tests path-tests)

;; ;; represent a graph by its successor function
;; (define (sucessors n g) (g n))


;; ;; Node -> ListOf<Node>
;; (define (rubik-successors n) 
;;   (cond ......horrible horrible .....))

;; (path? rubik-succesdors init-state final-state)

  
                         