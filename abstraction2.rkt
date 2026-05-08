;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstraction2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct IR [price name])
(define-struct address [first-name last-name street])
; An Addr is a structure: 
;   (make-address String String String)
; interpretation associates an address with a person's name
 
; [List-of Addr] -> String 
; creates a string from first names, 
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing l)
  (foldr string-append-with-space " "
         (sort (map address-first-name l) string<?)))
 
; String String -> String 
; appends two strings, prefixes with " " 
(define (string-append-with-space s t)
  (string-append " " s t))
 
(define ex0
  (list (make-address "Robert"   "Findler" "South")
        (make-address "Matthew"  "Flatt"   "Canyon")
        (make-address "Shriram"  "Krishna" "Yellow")))
 
(check-expect (listing ex0) " Matthew Robert Shriram ")

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))
              (foldl / 1 '(6 3 2)))
(define (f*oldl f e l)
  (foldr f e (reverse l)))

(define (build-l*st n f)
  (cond
    [(< n 0)(error "entrée invalide")]
    [(= n 0)(cons (f n) '())]
    [else (cons (f n) (build-l*st (- n 1) f))]))

; [List-of Addr] -> String 
; creates a string of first names, 
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing.v2 l)
  (local (; 1. extract names 
          (define names  (map address-first-name l))
          ; 2. sort the names 
          (define sorted (sort names string<?))
          ; 3. append them, add spaces 
          ; String String -> String
          ; appends two strings, prefix with " " 
          (define (helper s t)
            (string-append " " s t))
          (define concat+spaces
            (foldr helper " " sorted)))
    concat+spaces))

; [List-of Number] [Number Number -> Boolean] 
; -> [List-of Number]
; produces a version of alon0, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon) (isort (rest alon)))]))
 
          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon 
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))

(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
 

(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
  (local(
    ; Image NELoP -> Image
; connects the Posns in p in an image
         (define (connect-dots img p)
           (cond
             [(empty? (rest p)) img]
             [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))
 
         ; Image Posn Posn -> Image 
         ; draws a red line from Posn p to Posn q into im
         (define (render-line im p q)
           (scene+line
            im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
         )
    (render-line (connect-dots img p) (first p) (last p))))
 
 
; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
    

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; [Nelon [Number -> Boolean] -> Number]
(define (inf-1 l R)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local (
             (define (inf-in-rest l)
               (inf-1 (rest l) R)))
     (if (R (first l)
            (inf-in-rest l))
         (first l)
         (inf-in-rest l)))]))

(define (sup-1 l)
  (inf-1 l >))


; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define (extract-rest l)
                (extract1 (rest l))))
     (cond
       
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract-rest an-inv))]
       [else (extract-rest an-inv)]))]))

((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)

; [List-of Posn] -> [List-of Posn]
; adds 3 to each x-coordinate on the given list 
 
(check-expect
 (add-3-to-all
   (list (make-posn 3 1) (make-posn 0 0)))
 (list (make-posn 6 1) (make-posn 3 0)))
 
(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; adds 3 to the x-coordinate of p
          (define (add-3-to-x p)
            (make-posn (+ (posn-x p) 3) (posn-y p))))
    (map add-3-to-x lop)))

; Posn Posn Number -> Boolean
; is the distance between p and q less than d
(define (close-to p q d)
  (cond
    [(>= d (sqr (+ (sqrt (abs (- (posn-x p) (posn-x q)))) (sqrt (abs (- (posn-y p) (posn-y q)))))))#true]
    [[else #false]]))

; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt
 
(check-expect
 (close? (list (make-posn 47 54) (make-posn 0 60))
         (make-posn 50 50))
 #true)
 

(define (close? lop pt)
  (local (; Posn -> Boolean
          ; is one shot close to pt
          (define (is-one-close? p)
            (close-to p pt CLOSENESS)))
    (ormap is-one-close? lop)))
 
(define CLOSENESS 5) ; in terms of pixels 
