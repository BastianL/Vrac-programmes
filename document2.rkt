;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname document2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ball [location velocity])
(define-struct vel [deltax deltay])
(define ball1
  (make-ball (make-posn 30 40) (make-vel -10 5)))

(define-struct centry [name home office cell])
(define-struct phone  [area number])
(make-centry "shriram Fisler"
             (make-phone 101 1)
             (make-phone 101 "776-1099")
             (make-phone 208 "112-9981"))
; distances in terms of pixels:
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH  400)
(define CENTER (quotient WIDTH 2))
 
(define-struct game [left-player right-player ball])
 
(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))

(define-struct ufo [loc vel])

(define v1 (make-vel 8 -3))
(define v2 (make-vel -5 -3))

(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))
 
(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))


(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2)
              (make-ufo (make-posn 17 77) v2))

(define (posn+ p v)
  (make-posn (+ (posn-x p) (vel-deltax v))
             (+ (posn-y p) (vel-deltay v))))
 
(define (ufo-move-1 u)
  (make-ufo (posn+ (ufo-loc u) (ufo-vel u)) (ufo-vel u)))