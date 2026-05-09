;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editeur) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct editor [pre post])
; An Editor is a structure;
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

(define Back (empty-scene 200 20))

(define cursor (rectangle 1 20 "solid" "red"))

(define (textrender torand)
  (text torand 16 "black"))

(define (render editor) (overlay/align "left" "center" (beside(textrender (editor-pre editor)) cursor (textrender (editor-post editor)))Back))

(define (edit ed ke)
  