;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))

(define taille-cercle 10)

(define (circle2 outline color)
  (circle taille-cercle outline color))

(define (circle-red outline)
  (circle2 outline "red"))

(define (circle-green outline)
  (circle2 outline "green"))

(define (circle-yellow outline)
  (circle2 outline "yellow"))

(define (tl-render state)
  (place-image (circle-red (if [string=? state "red"] "solid" "outline"))15 15
  (place-image (circle-green (if [string=? state "green"] "solid" "outline")) 85 15 
  (place-image (circle-yellow (if [string=? state "yellow"] "solid" "outline")) 55 15
  (place-image (rectangle 200 60 "outline" "black") 0 0 (empty-scene 100 100))))))

(define (tl-next state)
  (cond [(string=? state "red") "green"]
  [(string=? state "green") "yellow"]
  [(string=? state "yellow") "red"]))

(define RED 0)
(define GREEN 1)
(define YELLOW 2)
 
; An S-TrafficLight is one of:
; – RED
; – GREEN
; – YELLOW

(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)
(check-expect (tl-next-numeric 0) 1)
(define (tl-next-numeric cs) (modulo (+ cs 1) 3))