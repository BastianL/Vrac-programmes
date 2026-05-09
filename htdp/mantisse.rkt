;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mantisse) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
       10 (* (inex-sign an-inex) (inex-exponent an-inex)))))
(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

(define inex1 (make-inex 95 1 2))
(define inex2 (make-inex 98 1 2))
(check-expect (inex+ inex1 inex2) (make-inex 19 1 3))

(define (inex+ i1 i2)
  (cond
    [(or (<= 100 (inex-mantissa i1)) (<= 100 (inex-mantissa i2))) (error "signal invalide")]
    [else (cond
            [(< 99 (+ (inex-mantissa i1) (inex-mantissa i2))) (make-inex (abs (/ (+ (inex-mantissa i1) (inex-mantissa i2)) 10)) (inex-sign i1) (+(inex-exponent i1) 1))]
            [else (make-inex (+ (inex-mantissa i1) (inex-mantissa i2))(inex-sign i1) (inex-exponent i1))])]))

(define (inex* i1 i2)
  (inexage (make-inex (* (inex-mantisse i1) (inex-mantisse i2)) (inex-sign i1) (+ (inex-exponant i1) (inex-exponant i2)))))

(define (inexage i)
  (cond
    [(> 99 (inex-exponent i)) (error "exponent trop élevé")]
    [(> 99 (inex-mantissa i)) (make-inex (abs (/ (inex-mantissa i) 10)) (inex-sign i) (add1 (inex-exponent i)))]))