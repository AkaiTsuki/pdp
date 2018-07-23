;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-1-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; draggable trees problem for ps04.

;; start with (run 1)

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; data structures

;; A World is a ListOf<Tree>

(define-struct node (x y selected? sons))

;; A Node is a Tree is a
;; (make-node Number Number Boolean Nodes)
;; x and y are the position of the center of the node, in pixels
;; selected? tells whether the node is selected
;; sons is the list of sons of the node, in any order.

;; A Nodes is either
;; -- empty
;; (cons Node Nodes)  
;; interp: a list of nodes


;; TEMPLATE:
;; node-fn : Node -> ??
;; (define (node-fn n)
;;   (... (node-x n) (node-y n) (node-selected? n) (sons-fn (node-sons n))))

;; nodes-fn : Nodes -> ??
;(define (nodes-fn ns)
;  (cond
;    [(empty? ns) ...]
;    [else (... (node-fn (first ns))
;               (nodes-fn (rest ns)))]))

;;;;;;;;;;;;;;;;

;; A TreeKeyEvent is a KeyEvent, which is one of
;; -- "t"   interp: add a new tree to the world
;; -- "n"   interp: add a new son to the selected node
;; -- "d"   interp: delete the tree rooted at the selected node
;; -- any other KeyEvent   interp: ignored

;; TEMPLATE:
;; tke-fn : TreeKeyEvent -> ??
;(define (tke-fn kev)
;  (cond
;    [(key=? kev "t") ...]
;    [(key=? kev "n") ...]
;    [(key=? kev "d") ...]
;    [else ...]))

;;;;;;;;;;;;;;;;

;; world-after-key-event : World TreeKeyEvent -> World
;; SD on kev : TreeKeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev "t") (world-with-new-tree w)]
    [(key=? kev "n") (world-with-new-son w)]
    [(key=? kev "d") (world-with-deleted-tree w)]
    [else w]))

;; ListOf<Tree> -> ListOf<Tree>
;; removes any tree that is selected from the list, then recur on the
;; sons of each surviving tree
;; HOFC + SD [Node]
(define (trees-filter-out-selected trees)
  (map
    (lambda (t)
      (make-node (node-x t) (node-y t) false
        (trees-filter-out-selected (node-sons t))))
    (filter
      (lambda (t) (not (node-selected? t)))
      trees)))

;; world-with-deleted-tree : World -> World
(define (world-with-deleted-tree w)
  (trees-filter-out-selected w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; World -> Scene
(define (world->scene w)
  (foldr
    place-tree
    EMPTY-CANVAS
    w))

;; Tree Scene -> Scene
;; produces a Scene like the original, but with an image of the tree added
(define (place-tree t scene)
  (place-image
    (node->image t)
    (node-x t)
    (node-y t)
    (foldr
      (lambda (son scene) (place-son son t scene))
      scene
      (node-sons t))))

;; Node Node Scene -> Scene
;; Given a son and its parent node, returns a scene in which
;; the son and its subtrees, and the blue line from the parent to the
;; son have been added.
(define (place-son son parent scene)
  (add-line
    (place-tree son scene)  ;; place the son & its subtree
    (node-x parent) (node-y parent)
    (node-x son) (node-y son)
    "blue"))

(define (node->image t)
  (if (not (node-selected? t))
    NODE-IMAGE-UNSELECTED
    (if (node-room-for-sons? t)
      NODE-IMAGE-SELECTED
      NODE-IMAGE-NO-ROOM)))

    



      