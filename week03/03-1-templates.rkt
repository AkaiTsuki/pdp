;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname templates) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;;; A NonEmptyList<X> is either
;;; (cons X empty)
;;; (cons X NonEmptyList<X>)

;;; nel-fn : NonEmptyList<X> -> ??
;;; livecoding!
  

;;; last-sardine : NonEmptyList<Sardine> -> Sardine
;;; livecoding!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct bintree (left data right))
(define-struct leaf (data))

;;; A BinTree<X> is either
;; -- empty
;; -- (make-bintree BinTree<X> X BinTree<X>)

;;; bintree-fn : BinTree<X> -> ???
;;; livecoding!

;;; A CatAnimationKeyEvent is a KeyEvent that is one of
;;; " "       interp: pause the animation
;;; "n"       interp: create a new cat
;;; "k"       interp: kill the selected cat if there is one
;;; any other one-character key event  interp: display a "?" on the screen
;;; "left"    interp: move the selected cat left if there is one
;;; "right"   interp: move the selected cat right if there is one
;;; "drop"    interp: drop the selected cat to the bottom 
;;; any other KeyEvent

;;; cake-fn : CatAnimationKeyEvent -> ???
(define (cake-fn kev)
  (cond
    [(key=? kev " ") ...]
    [(key=? kev "n") ...]
    [(key=? kev "k") ...]
    [(= (string-length kev) 1) ...]
    [(key=? kev "left") ...]
    [(key=? kev "right") ...]
    [(key=? kev "drop") ...]
    [else ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
