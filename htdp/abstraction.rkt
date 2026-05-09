;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Lon (List-of-numbers) 
; is one of:
; – '() 
; – (cons Number Lon) 
  

; An Los (List-of-String) 
; is one of: 
; – '() 
; – (cons String Los)  

; A [List-of ITEM] is one of: 
; – '() 
; – (cons ITEM [List-of ITEM])

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

; A List-of-numbers-again is one of: 
; – '() 
; – (cons Number List-of-numbers-again)

(define-struct point [hori veri])

; A Pair-boolean-string is a structure:
;   (make-point Boolean String)
 
; A Pair-number-image is a structure:
;   (make-point Number Image)

; A [CP H V] is a structure: 
;   (make-point H V)

; A [List X Y] is a list: 
;   (cons X (cons Y '()))

; A [List-of-Number X Y] is a list:
;   (cons X (cons Y '()))
; where X and Y are number.

(cons 4 (cons 3 '()))
; A [Pair-of-Number-and-Letter X Y] is a list:
;   (cons X (cons Y '()))
; where X is a number and Y a 1string.

(cons 4 (cons "l" '()))
(cons "bac" (cons #true '()))

; [List-of [CP Boolean Image]]

; An LStr is one of: 
; – String
; – (make-layer LStr)
    
; An LNum is one of: 
; – Number
; – (make-layer LNum)

(define-struct layer [stuff])

; An Lstuff is one of :
; - ITEM
; - (make-layer Lstuff)

; A NElist-of is one of
; -(cons ITEM '())
; -(cons ITEM NElist-of)

; A List-of [Maybe String] is one of
; '()
; (cons [MaybeString] list-of[Maybe String])

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #false]
    [(string=? (first los) s)(rest los)]
    [else (occurs s (rest los))]))

(define (function=at-1.2-3-and-5.775? f1 f2)
  (if (and (= (f1 1.2) (f2 1.2)) (= (f1 3) (f2 3))(= (f1 5.775) (f2 5.775)))#true #false))
(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l)
                   (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))
(define (f x) x)
(cons f '())
(f f)
(cons f (cons 10 (cons (f 10) '())))
