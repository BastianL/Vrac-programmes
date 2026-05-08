;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname voiture) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define WIDTH-OF-WORLD 200)
 
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle (* WHEEL-RADIUS 2) WHEEL-RADIUS "solid" "white"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR
  (underlay/align/offset "center" "bottom" (rectangle (+ (image-width BOTH-WHEELS) WHEEL-RADIUS) (* WHEEL-RADIUS 4) "solid" "red") 0 WHEEL-RADIUS BOTH-WHEELS))

(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

(define BACKGROUND (place-image tree 10 (image-height tree) (empty-scene WIDTH-OF-WORLD (+ (image-height tree) 20))))

(define Y-CAR (- (+ (image-height tree) (image-height CAR))WHEEL-RADIUS))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 
 (define (render cw)
   (place-image/align CAR cw Y-CAR "right" "bottom" BACKGROUND))

; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
; examples: 
;   given: 20, expect 23
;   given: 78, expect 81
(define (tock cw)
  (if (< cw WIDTH-OF-WORLD) (+ cw 3) cw))

; WorldState -> WorldState
; launches the program from some initial state 

; Number -> Image
; An AnimationState is a Number.
; interpretation the number of clock ticks 
; since the animation started

(define (tock2 AS)
  (+ AS 1))

;AnimationState->Image
;Déplace la voiture de 3 pixels par tick
(define (render2 AS)
  (place-image/align CAR (if (< (+ (* (sin AS) 3) (/ WIDTH-OF-WORLD 2)) WIDTH-OF-WORLD) (+ (* (sin AS) 3) (/ WIDTH-OF-WORLD 2)) WIDTH-OF-WORLD) Y-CAR "right" "bottom" BACKGROUND))

(define (main ws)
   (big-bang ws
     [on-tick tock]
     [on-mouse hyper]
     [to-draw render]))


; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
; given: 21 10 20 "enter"
; wanted: 21
; given: 42 10 20 "button-down"
; wanted: 10
; given: 42 10 20 "move"
; wanted: 42
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))