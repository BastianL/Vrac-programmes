;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ovni) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 300)
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay (circle 10 "solid" "green") (rectangle 50 10 "solid" "green")))

(define (main y0)
  (big-bang y0
    [on-tick nxt]
    [to-draw render/status]))

(define (nxt y)
  (+ y 3))

(define (render y)
  (place-image UFO (/ WIDTH 2) y MTSCN))

(define (render/status y)
  (place-image
   (cond
     [(<= 0 y CLOSE)
      (text "descending" 11 "green")]
     [(and (< CLOSE y) (<= y HEIGHT))
      (text "closing in" 11 "orange")]
     [(> y HEIGHT)
      (text "landed" 11 "red")])
   20 20
   (render y)))