;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname position) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

(define (main p0)
  (big-bang p0
    [on-tick x+]
    [on-mouse reset-dot]
    [to-draw scene+dot]))

(define (scene+dot p)
  (place-image DOT (posn-x p)(posn-y p) MTS))

(define (x+ p)
  (make-posn (+ (posn-x p) 3) (posn-y p)))

;Posn+Number-> Posn
;replace the x-coordinate by the number

(check-expect (posn-up-x (make-posn 12 5) 4) (make-posn 4 5))
(define (posn-up-x p n)
  (make-posn n (posn-y p)))

(define (reset-dot p x y me)
  (cond
    [(mouse=? "button-down" me)(make-posn x y)]
    [else p]))
