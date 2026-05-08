;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstractiondot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HAUTEUR 200)
(define LARGEUR HAUTEUR)
(define MT-SCENE (empty-scene HAUTEUR LARGEUR))
(define DOT (circle 5 "solid" "red"))
; foldr : [X Y] [X Y -> Y] Y [List-of X] -> Y
; foldl : [X Y] [X Y -> Y] Y [List-of X] -> Y

; [List-of Posn] -> Image 
; adds the Posns on lop to the empty scene 

(check-expect (dots (list (make-posn 12 31)))
              (place-image DOT 12 31 MT-SCENE))

(define (dots lop)
  (local (; Posn Image -> Image 
          ; adds a DOT at p to scene
          (define (add-one-dot p scene)
            (place-image DOT
                         (posn-x p) (posn-y p)
                         scene)))
    (foldr add-one-dot MT-SCENE lop)))

(check-expect (convertFC (list -40 0 32)) (list -40 -17.78 0))
;list-of-number -> list-of-number
;transforme une liste de nombres en une autre liste de nombre par addition et soustraction
(define (convert l t p)
  (local ((define (conversion n)
            (+ (* n t) p)))
  (map conversion l)))

(define (convertFC l)
  (convert l (/ 5 9) -32))

(define (translate l)
  (cond
    [(empty? l)'()]
    [else (cons (list (posn-x (first l)) (posn-y (first l))) (translate (rest l)))]))

(define-struct inventory-record [name desc acq rec])


(define a (make-inventory-record "a" "a" 5 7))
(define b (make-inventory-record "b" "b" 9 12))
(define c (make-inventory-record "c" "c" 20 21))
(define d (make-inventory-record "d" "d" 100 200))

(check-expect (benefit (list a b c d)) (list c a b d))
;list-of-inventory record -> list-of-inventory-record
;classe les inventory record par inventory-record-rec - inventory-record-acq
(define (benefit lir)
  (local ((define (benefit-calc ir)
            (- (inventory-record-rec ir) (inventory-record-acq ir)))
          (define (benefit-calc-2 a b)
            (< (benefit-calc a) (benefit-calc b))))
    (sort lir benefit-calc-2)))

;[number list-of-inventary-record -> list-of-inventory-record]
;crée une liste constituée des inventory-records dont le rec est en dessous ou égal a ua
(define (eliminate-expensive ua lir)
  (cond
    [(empty? lir) '()]
    [else (if (<= (inventory-record-rec (first lir)) ua) (cons (first lir) (eliminate-expensive ua (rest lir)))(eliminate-expensive ua (rest lir)))]))

(define (recall ty lir)
  (local ((define (recall-check ir)
            (not (string-contains-ci? ty (inventory-record-name ir)))))
    (filter recall-check lir)))

(check-expect (selection (list 1 2 3 4 5) (list 5 3 7 2 1)) (list 1 2 3 5))

(define (selection l1 l2)
  (local ((define (select n1)
            (member n1 l2)))
    (filter select l1)))

(define (basic-list n)
  (local ((define (no-function n)
    n))
  (build-list n no-function)))

(define (inc-list n)
  (build-list n add1))

(define (inv-list n)
  (local ((define (inv-function n)
            (/ 1 (add1 n))))
    (build-list n inv-function)))

(define (even-list n)
  (local ((define (even-function n)
            (* n 2)))
    (build-list n even-function)))

(define (MatriceM n)
  (local (;n indique le nombre total d'élément, o indique où il faut mettre le 1, p indique où on en est sur la ligne
          (define (matrice-function o p)
            (cond
              [(= p n) '()]
              [(= p o) (cons 1 (matrice-function o (+ p 1)))]
              [else (cons 0 (matrice-function o (+ p 1)))]))
          (define (matrice-function-init o)
            (matrice-function o 0)))
    (build-list n matrice-function-init)))
;donne R pour n entre 0 et n
(define (tabulate R n)
  (reverse (build-list (add1 n) R)))

;prend un nom et une liste de nom, donne un bolean qui dit si un des noms de la liste est égal ou contenu au nom donné en exemple
(define (find-name n lon)
  (local ((define (name-check n1)
            (string-contains-ci? n n1)))
    (ormap name-check lon)))

(define (operation R l)
  (foldr R l))

(define (list-sum l)
  (operation + l))

(define (list-product l)
  (operation * l))

(define (image-comp l)
  (foldr beside l empty-image))

(define (image-vert l)
  (foldr above l empty-image))


(define (prefixes l)
  