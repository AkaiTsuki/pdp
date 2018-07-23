#lang racket

;; 8-6-shapes-with-interfaces.rkt

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)

;; all geometric shapes support these methods in all contexts
;; a Shape is an object of a class that implements Shape<%>.
(define Shape<%> 
  (interface ()

    ;; -> Number
    ;; compute the weight of this shape
    weight 

    ;; Scene -> Scene
    ;; add this shape to the given scene 
    add-to-scene 

    ))

;; A Circle is a (new Circle% [x Number][y Number][r Number][c ColorString])
;; represents a circle 
(define Circle%
  (class* object% (Shape<%>)
    (init-field x  ; Number, x pixels of center from left
                y  ; Number, y pixels of center from top
		r  ; Number, radius
		c) ; ColorString 

    (field [IMG (circle r "solid" c)])

    (super-new)

    ;; strategy: domain knowledge
    (define/public (weight) (* pi r r))

    ;; strategy: domain knowledge
    (define/public (add-to-scene s) (place-image IMG x y s))

    ))

;; A Square is a (new Square% [x Number][y Number][l Number][c ColorString])
;; represents a square parallel to sides of canvas 
(define Square%
  (class* object% (Shape<%>)
    (init-field x  ; Number, x pixels of center from left
                y  ; Number, y pixels of center from top
		l  ; Number, length of one side 
		c) ; ColorString 

    (field [IMG (rectangle l l "solid" c)])

    (super-new)

    ;; strategy: domain knowledge
    (define/public (weight) (* l l))

    ;; strategy: domain knowledge
    (define/public (add-to-scene s) (place-image IMG x y s))

    ))

;; A Composite is a (new Composite% [front Shape][back Shape])
;; a composite of front and back
(define Composite%
  (class* object% (Shape<%>)
     (init-field front  ; Shape, the shape in front
                 back   ; Shape, the shape in back
                 )

     (super-new)

     ;; all we know here is that front and back implement Shape<%>.
     ;; we don't know if they are circles, squares, or other composites!

     ;; strategy: structural decomposition on this
     (define/public (weight) (+ (send front weight)
                                (send back weight)))

     ;; strategy: structural decomposition on this + acc [scene]
     (define/public (add-to-scene scene)
       (send front add-to-scene
             (send back add-to-scene scene)))
     
     ))

(define EMPTY-CANVAS (empty-scene 200 100))

(define s1 (new Circle% [x 50][y 20][r 40][c "red"]))
(define s2 (new Square% [x 80][y 70][l 40][c "blue"]))
(define s3 (new Composite% [front s1][back s2]))


(define-test-suite function-tests
  (check-= (send s1 weight) (* pi 1600) .1)
  (check-equal? (send s2 weight) 1600)
  (check-= (send s3 weight) (+ (* pi 1600) 1600) .1))

(run-tests function-tests)

