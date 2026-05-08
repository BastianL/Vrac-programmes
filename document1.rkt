;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname document1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(check-expect (distance-to-0 (make-posn 3 4)) 5)
(check-expect (distance-to-0 (make-posn 8 6)) 10)
(check-expect (distance-to-0 (make-posn 5 12)) 13)

(define (distance-to-0 ap) (sqrt(+(sqr(posn-x ap)) (sqr(posn-y ap)))))

(define (manhattan-distance ap)
  (+(posn-x ap) (posn-y ap)))

(define-struct r3 [x y z])

(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))

(check-expect (r3-distance-to-0 (make-r3 2 3 6)) 7)
(define (r3-distance-to-0 p)
  (sqrt(+(sqr(r3-x p)) (sqr(r3-y p)) (sqr(r3-z p))))) 