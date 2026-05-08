;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname SIGS) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
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

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

(define initial-state (make-aim (make-posn (/ WIDTH-OF-WORLD 2) 50) (make-tank (/ WIDTH-OF-WORLD 2) speed-tank)))

  
; re
(make-aim (make-posn 20 10) (make-tank 28 -3))

(make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT-OF-WORLD TANK-HEIGHT)))

(make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103))
; SIGS -> Image
; adds TANK, UFO, and possibly MISSILE to 
; the BACKGROUND scene
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
       (fired-tank s)
       (ufo-render (fired-ufo s)
                   (missile-render (fired-missile s)
                                   BACKGROUND)))]))

; Tank Image -> Image 
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK-RENDER (tank-loc t) (- HEIGHT-OF-WORLD TANK-HEIGHT) im)
    )
 
; UFO Image -> Image 
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO-RENDER (posn-x u)(posn-y u) im))

; Missile Image -> Image 
; adds m to the given image im
(define (missile-render m im)
  (place-image missile (posn-x m) (posn-y m) im))

;Arrête le jeu (le missile connecte ou l'ufo touche le sol)
;prend donc la position de l'ufo et du missile
(define (si-game-over? u m)
  (cond
    [(>= (posn-y u) HEIGHT-OF-WORLD) #true]
    [(and (= (posn-x u) (posn-x m))(= (posn-y u)(posn-y m)))#true]
    [else #false]))

(define (si-render-final s)
  (overlay (text "you win" 20 "black") (si-render s)))

;définit quels objets vont bouger dans sigs
(define (si-move w)
  (si-move-proper w (- 3 (random 7))))
 
; SIGS Number -> SIGS 
; moves the space-invader objects predictably by delta
; moves the ufo according to random and vertically by Speed-ufo
(define (si-move-proper w delta)
  (cond
    [(aim? w) (make-aim (make-posn (+ (posn-x (aim-ufo w)) delta)(+ (posn-y (aim-ufo w)) speed-ufo))(make-tank (+ (tank-loc (aim-tank w))(tank-vel (aim-tank w))) (tank-vel (aim-tank w))))]
    [(fired? w) (make-fired (make-posn (+ (posn-x (fired-ufo w)) delta)(+ (posn-y (fired-ufo w)) speed-ufo))(make-tank (+ (tank-loc (fired-tank w))(tank-vel (fired-tank w)))(tank-vel (fired-tank w)))(make-posn (posn-x (fired-missile w))(-(posn-y (fired-missile w))speed-missile)))]))

(define (si-control w ke)
  (cond
    [(fired? w)
      (cond
        [(string=? "left" ke)(make-fired (fired-ufo w)(make-tank (tank-loc (fired-tank w)) (* speed-tank -1))(fired-missile w))]
        [(string=? "right" ke)(make-fired (fired-ufo w)(make-tank (tank-loc (fired-tank w)) speed-tank)(fired-missile w))]
        [(string=? "space" ke) w]
        [else w])]
    [(aim? w)
     (cond
        [(string=? "left" ke)(make-aim (aim-ufo w)(make-tank (tank-loc (aim-tank w)) (* speed-tank -1)))]
        [(string=? "right" ke)(make-aim (aim-ufo w)(make-tank (tank-loc (aim-tank w)) speed-tank))]
        [(string=? " " ke) (make-fired (aim-ufo w)(aim-tank w) (make-posn (tank-loc (aim-tank w))(- HEIGHT-OF-WORLD(+ TANK-HEIGHT 3))))]
        [else w])]))

(define (si-main w)
 (big-bang w
   [on-tick si-move]
   [on-key si-control]
   [to-draw si-render]))

(si-main initial-state)