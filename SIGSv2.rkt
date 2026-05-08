;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname SIGSv2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(define TANK-HEIGHT 10)
(define WIDTH-OF-WORLD 200)
(define UFO-RENDER (overlay (rectangle 10 2 "solid" "green") (circle 3 "solid" "green")))
(define TANK-RENDER (rectangle 20 TANK-HEIGHT "solid" "red"))
(define missile (triangle 3 "solid" "red"))
(define tree (above/align "middle" (ellipse 40 60 "solid" "brown")(line 0 20 "black")))
(define HEIGHT-OF-WORLD 200)
(define BACKGROUND (place-image tree (* (/ WIDTH-OF-WORLD 4) 3) (- HEIGHT-OF-WORLD (/ (image-height tree)2))(empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))
(define speed-ufo 0.1)
(define speed-missile 1)
(define speed-tank 2)
; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])

; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place


(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
 
; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

; SIGS Number -> SIGS 
; moves the space-invader objects predictably by delta
; moves the ufo according to random and vertically by Speed-ufo
(define (si-move-proper w delta)
  (cond
    [(boolean? (sigs-missile w)) (make-sigs (make-posn (+ (posn-x (sigs-ufo w)) delta)(+ (posn-y (sigs-ufo w)) speed-ufo))(make-tank (+ (tank-loc (sigs-tank w))(tank-vel (sigs-tank w))) (tank-vel (sigs-tank w))) #false )]
    [(posn? (sigs-missile w)) (make-sigs (make-posn (+ (posn-x (sigs-ufo w)) delta)(+ (posn-y (sigs-ufo w)) speed-ufo))(make-tank (+ (tank-loc (sigs-tank w))(tank-vel (sigs-tank w)))(tank-vel (sigs-tank w)))(make-posn (posn-x (sigs-missile w))(-(posn-y (sigs-missile w))speed-missile)))]))

;définit quels objets vont bouger dans sigs
(define (si-move w)
  (si-move-proper w (- 3 (random 7))))

(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [(posn? m) (place-image missile (posn-x m) (posn-y m) s)]))

(define (ufo-render u im)
  (place-image UFO-RENDER (posn-x u)(posn-y u) im))

; Tank Image -> Image 
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK-RENDER (tank-loc t) (- HEIGHT-OF-WORLD TANK-HEIGHT) im))

; SIGS.v2 -> Image 
; renders the given game state on top of BACKGROUND 
(define (si-render.v2 s)
  (tank-render
    (sigs-tank s)
    (ufo-render (sigs-ufo s)
                (missile-render.v2 (sigs-missile s)
                                   BACKGROUND))))

(define (si-main w)
 (big-bang w
   [check-with sigs?]
   [on-tick si-move]
   [on-key si-control]
   [to-draw si-render.v2]))
 

(define (si-control w ke)
  (cond
    [(posn? (sigs-missile w))
      (cond
        [(string=? "left" ke)(make-sigs (sigs-ufo w)(make-tank (tank-loc (sigs-tank w)) (* speed-tank -1))(sigs-missile w))]
        [(string=? "right" ke)(make-sigs (sigs-ufo w)(make-tank (tank-loc (sigs-tank w)) speed-tank)(sigs-missile w))]
        [(string=? " " ke) w]
        [else w])]
    [(boolean? (sigs-missile w))
     (cond
        [(string=? "left" ke)(make-sigs (sigs-ufo w)(make-tank (tank-loc (sigs-tank w)) (* speed-tank -1))#false)]
        [(string=? "right" ke)(make-sigs (sigs-ufo w)(make-tank (tank-loc (sigs-tank w)) speed-tank)#false)]
        [(string=? " " ke) (make-sigs (sigs-ufo w)(sigs-tank w) (make-posn (tank-loc (sigs-tank w))(- HEIGHT-OF-WORLD(+ TANK-HEIGHT 3))))]
        [else w])]))

(define initial-state (make-sigs (make-posn (/ WIDTH-OF-WORLD 2) 50) (make-tank (/ WIDTH-OF-WORLD 2) speed-tank)#false))
