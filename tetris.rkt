;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally 
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE)) ;size of a scene for render
(define HEIGHT 10)
(define SCENE-HEIGHT (* HEIGHT SIZE))
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])

; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

(define block-landed (make-block 0 (- HEIGHT 1)))

(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape0 (list block-landed block-on-block))
(define tetris0 (make-tetris (make-block 5 0) landscape0))
(define block-to-ground (make-tetris (make-block 5 9) '()))

;consomme un tetris, produit une image
;parcours la liste et place des blocs a chaque emplacement spécifié de landscape + le bloc dans bloc.

(define (tetris-render tetr)
  (cond
    [(empty? (tetris-landscape tetr)) (place-block (tetris-block tetr) (empty-scene SCENE-SIZE SCENE-SIZE))]
    [else (place-block (first (tetris-landscape tetr)) (tetris-render (make-tetris (tetris-block tetr) (rest (tetris-landscape tetr)))))]))
;consomme un block et une scene, place le bloc dans la scène
(define (place-block bloc scene)
  (place-image BLOCK (+ (* (block-x bloc) SIZE)(/ SIZE 2)) (+ (* (block-y bloc) SIZE) (/ SIZE 2)) scene))

(define (block-dropping b) (make-block (block-x b) (+ (block-y b) 1)))

(define (tetris-main t)
  (tetris-game (make-tetris (make-block (abs (/ WIDTH 2)) 0) '()) t))

(define (tetris-game cs t)
  (big-bang cs
    [to-draw tetris-render]
    [on-tick tock t]
    [on-key keyreader]
    [stop-when lastword]))

(define (lastword cs)
  (>= (topblockcount (tetris-landscape cs)) 2))

(define (topblockcount land)
  (cond
    [(empty? land) 0]
    [else (+ (topblockcount (rest land)) (if (= 0 (block-y (first land))) 1 0))])) 
(define (attemptmove blocx dir )
  (cond
    [(string=? dir "right")(if (< (+ blocx 1) WIDTH) (+ blocx 1) 0)]
    [(string=? dir "left")(if (>= (- blocx 1) 0) (- blocx 1) WIDTH)]))

(define (blocklanded cs)
  (make-tetris (make-block (attemptmove (block-x (tetris-block cs)) "right") 0) (cons (tetris-block cs) (tetris-landscape cs))))

(check-expect (blocklanded block-to-ground) (make-tetris (make-block 6 0) (list (make-block 5 9))))

(check-expect (tock block-to-ground) (make-tetris (make-block 6 0) (list (make-block 5 9))))
(define (tock cs)
  (cond
    [(member? (block-dropping (tetris-block cs)) (tetris-landscape cs))(blocklanded cs)]
    [(= (block-y (block-dropping (tetris-block cs))) HEIGHT)(blocklanded cs)]
    [else (make-tetris (block-dropping (tetris-block cs)) (tetris-landscape cs))]))

(define (keyreader cs string)
  (cond
    [(string=? string "right")(make-tetris (make-block (if (check-block cs string) (+ (block-x (tetris-block cs)) 1) (block-x (tetris-block cs))) (block-y (tetris-block cs))) (tetris-landscape cs))]
    [(string=? string "left")(make-tetris (make-block (if (check-block cs string) (- (block-x (tetris-block cs)) 1) (block-x (tetris-block cs))) (block-y (tetris-block cs))) (tetris-landscape cs))]
    [else cs]))

(check-expect (check-block (make-tetris (make-block 5 0) (list (make-block 6 0))) "right") #false)

(define (check-block cs string)
  (cond
    [(string=? string "right") (and (< (+ (block-x (tetris-block cs)) 1) WIDTH)(not (member? (make-block (+ (block-x (tetris-block cs)) 1) (block-y (tetris-block cs))) (tetris-landscape cs))))]
    [(string=? string "left") (and (>= (- (block-x (tetris-block cs)) 1) 0)(not (member? (make-block (- (block-x (tetris-block cs)) 1) (block-y (tetris-block cs))) (tetris-landscape cs))))]))