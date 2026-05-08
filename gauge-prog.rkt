;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gauge-prog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; prend le bonheur actuel, 100 pour l'initiailisation
; renvoie une image d'un rectangle avec une barre correspondant au bonheur actuel.
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [on-key change]
    [to-draw render]))

;diminue le bonheur de 0,1 par seconde
(define (tock ws)
  (if (< (- ws 0.1) 0) 0 (- ws 0.1)))

;modifie le bonheur en fonction de la touche pressée
(define (change ws a-key)
  (cond
    [(key=? a-key "up") (if (> (+ ws 33) 100) 100 (+ ws 33))]
    [(key=? a-key "down") (if (< (- ws 20) 0) 0 (- ws 20))]
    [else ws]))
;dessine un rectangle de la taille correspondante au bonheur actuel.
;par-dessus un rectangle 
(define (render ws)
  (underlay/align "center" "bottom" (rectangle 20 ws "solid" "red") (rectangle 20 100 "outline" "black")))