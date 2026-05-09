;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
;interpretaton seg is a list of worm-segment. dir is the direction it's currently aiming.
(define-struct worm [seg dir])
(define worm-diameter 5)
(define MAX 20)
(define redcirc (circle (/ worm-diameter 2) "solid" "red"))
(define world-width (* MAX worm-diameter))
(define world-height (* MAX worm-diameter))
(define greencirc (circle (/ worm-diameter 2) "solid" "green"))

(define-struct game [worm food])
;a game is a struct
;interpretation the worm and the pos of the food
;A worm with tail is either
; '()
; connected segment worm with tail
; interpretation each connected segment represent one of the segment of the worm.

; a connected segment is
; pos
; interpretation a segment that differ of his predecossor by only one direction
(define (render cs)
  (cond
    [(empty? (worm-seg (game-worm cs))) (place-image greencirc (* (posn-x (game-food cs)) worm-diameter) (* (posn-y (game-food cs)) worm-diameter) (empty-scene world-width world-height))]
    [else (place-image redcirc (* (posn-x (first (worm-seg (game-worm cs)))) worm-diameter) (* (posn-y (first (worm-seg (game-worm cs)))) worm-diameter) (render (make-game (make-worm (rest (worm-seg (game-worm cs))) (worm-dir (game-worm cs))) (game-food cs))))]))

(define (reader cs key)
  (cond
    [(string=? key "left")(make-game (make-worm (worm-seg (game-worm cs)) "left") (game-food cs))]
    [(string=? key "right")(make-game (make-worm (worm-seg (game-worm cs)) "right") (game-food cs))]
    [(string=? key "up")(make-game (make-worm (worm-seg (game-worm cs)) "up") (game-food cs))]
    [(string=? key "down")(make-game (make-worm (worm-seg (game-worm cs)) "down") (game-food cs))]))

(define (wormgame p)
  (big-bang p
    [to-draw render]
    [on-key reader]
    [on-tick tock 1]
    [stop-when borderhit renderfinal]))

(define (borderhit cw)
  (or (= (posn-x (first (worm-seg (game-worm cw)))) 0) (= (* (posn-x (first (worm-seg (game-worm cw)))) worm-diameter) world-width) (= (posn-y (first (worm-seg (game-worm cw)))) 0) (= (* (posn-y (first (worm-seg (game-worm cw)))) worm-diameter) world-height) (member? (first (worm-seg (game-worm cw))) (rest (worm-seg (game-worm cw))))))

(define (tock cs)
 (cond
   [(string=? "left" (worm-dir (game-worm cs)))(make-game (make-worm (cons (make-posn (- (posn-x (first (worm-seg (game-worm cs)))) 1) (posn-y (first (worm-seg (game-worm cs)))))
                                                    (wormgrowth cs)) "left") (foodpos cs))]
   [(string=? "right" (worm-dir (game-worm cs)))(make-game (make-worm (cons (make-posn (+ (posn-x (first (worm-seg (game-worm cs)))) 1) (posn-y (first (worm-seg (game-worm cs)))))
                                                    (wormgrowth cs)) "right") (foodpos cs))]
   [(string=? "up" (worm-dir (game-worm cs)))(make-game (make-worm (cons (make-posn (posn-x (first (worm-seg (game-worm cs)))) (- (posn-y (first (worm-seg (game-worm cs)))) 1))
                                                             (wormgrowth cs)) "up") (foodpos cs))]
   [(string=? "down" (worm-dir (game-worm cs))) (make-game (make-worm (cons (make-posn (posn-x (first (worm-seg (game-worm cs)))) (+ (posn-y (first (worm-seg (game-worm cs)))) 1))
                                                             (wormgrowth cs)) "down") (foodpos cs))]))

(define (wormgrowth cs)
  (if (checkfood cs) (worm-seg (game-worm cs)) (allbutlast (worm-seg (game-worm cs)))))

(define (checkfood cs)
 (and(= (posn-x (first (worm-seg (game-worm cs))))(posn-x (game-food cs)))(= (posn-y (first (worm-seg (game-worm cs))))(posn-y (game-food cs)))))
;prend un état, donne une position pour la nouriture
(define (foodpos cs)
  (if (checkfood cs) (make-posn (random MAX) (random MAX)) (game-food cs))) 
; Posn -> Posn 
; try to create a new food at a posn between the canvas
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
     p (make-posn (random MAX) (random MAX))))
 
; Posn Posn -> Posn 
; generative recursion 
; check if the snake is at the same place that the food, and create a food if 
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

(check-expect (allbutlast (list 3 2 1)) (list 3 2))
(define (allbutlast lis)
  (cond
    [(empty? (rest lis)) '()]
    [else (cons (first lis) (allbutlast (rest lis)))]))

; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

(define launcher (make-game (make-worm (list (make-posn 10 10) (make-posn 10 9) (make-posn 10 8) (make-posn 10 7) (make-posn 10 6) (make-posn 10 5)) "left") (make-posn 10 15)))

(define (renderfinal cs)
  (overlay/align "left" "bottom" (text (if (member? (first (worm-seg (game-worm cs))) (rest (worm-seg (game-worm cs))))"worm hit itself" "worm hit border") 12 "black") (render cs)))

