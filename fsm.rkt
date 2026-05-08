;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; ExpectsToSee.v1 is one of: 
; – "start, expect an 'a'"
; – "expect 'b', 'c', or 'd'"
; – "finished" 
; – "error, illegal key" 

(define (fsm ws)
  (big-bang ws
    (to-draw render)
    (on-key kereader)))

; render affiche un rectanble blanc dans l'etat initial, un rectangle jaune dans le second etat, un rectangle vert dans l'etat final, et un rectangle rouge dans lretat erreur
(define (render ws)
  (rectangle 100 100 "solid" (cond
    [(string=? ws "start, expect an 'a'")"white"]
    [(string=? ws "expect 'b', 'c', or 'd'")"yellow"]
    [(string=? ws "finished")"green"]
    [(string=? ws "error, illegal key")"red"])))

(define (kereader ws ke)
  (cond
    [(string=? ws "start, expect an 'a'")(if (string=? "a" ke) "expect 'b', 'c', or 'd'" "error, illegal key")]
    [(string=? ws "expect 'b', 'c', or 'd'")
     (cond
       [(or (string=? "b" ke)(string=? "c" ke)) ws]
       [(string=? "d" ke) "finished"]
       [else "error, illegal key"])]
    [(string=? ws "finished") ws]
    [(string=? ws "error, illegal key") ws]))

; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))

(define MESSAGE
  "traffic light expected, given some other value")

(define MESSAGE1
  "first value isn't a traffic light")

(define MESSAGE2
  "second value isn't a traffic light")

(define MESSAGE3
  "both values aren't traffic light")

; Any Any -> Boolean
; are the two values elements of TrafficLight and, 
; if so, are they equal
 
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
 
(define (light=? a-value another-value)
  (cond
    [(light? a-value)(if(light? another-value)(string=? a-value another-value)(error MESSAGE2))]
    [(light? another-value)(error MESSAGE1)]
    [else (error MESSAGE3)]))