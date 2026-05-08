;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editeur2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t
(define (Editor? edi)
  (cond
    [(and(string? (editor-pre edi))(string? (editor-post edi)))#true]
    [else #false]))
;editor -> image
;create an image of the two strings of the editor in a rectangle with a selector between both strings.

(define cursor (rectangle 1 20 "solid" "red"))

(define (render edi)
  (overlay/align  "left" "center" (beside (text (editor-pre edi) 11 "black") cursor (text (editor-post edi) 11 "black")) (empty-scene 200 20)))

;editor + keyevent -> editor
;interpretation : take an existing editor and turns it into another editor with the keyevent ke added to pre, or delete the last character if the caracter is backspace, or change the position of the pre and post field.
(check-expect (edit (make-editor "" "world") "left")(make-editor "" "world"))
(check-expect (edit (make-editor "world" "") "right")(make-editor "world" ""))
(check-expect (edit (make-editor "hello" "world") "left")(make-editor "hell" "oworld"))
(check-expect (edit (make-editor "hello" "world") "right")(make-editor "hellow" "orld"))
(check-expect (edit (make-editor "ok" "cool") "\b")(make-editor "o" "cool"))
(check-expect (edit (make-editor "" "cool") "\b")(make-editor "" "cool"))
(check-expect (edit (make-editor "kdgh" "") "left")(make-editor "kdg" "h"))
(define (edit ed ke)
  (cond
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(string=? "\b" ke) (if (= (string-length (editor-pre ed)) 0) ed (make-editor (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1)) (editor-post ed)))]
    [(string=? "left" ke) (if (= (string-length (editor-pre ed)) 0) ed (make-editor (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1)) (string-append (string-ith (editor-pre ed) (- (string-length (editor-pre ed)) 1)) (editor-post ed))))]
    [(string=? "right" ke) (if (= (string-length (editor-post ed)) 0) ed (make-editor (string-append (editor-pre ed) (string-ith (editor-post ed) 0)) (substring (editor-post ed) 1 (string-length (editor-post ed)) )))]
    [(= (string-length ke) 1) (if (> (image-width (text (string-append (editor-pre ed) ke (editor-post ed)) 11 "black" )) 200) ed (make-editor (string-append (editor-pre ed) ke) (editor-post ed)))]
    [else ed]))

(define (run pre)
  (big-bang pre
    (check-with Editor?)
    (to-draw render)
    (on-key edit)))

; an editor2 is a structure
; (make-editor string number)
; where string is the text inside the editor, and number where the cursor is placed
(define-struct editor2 [text index])

;editor->image
;create a text image with a cursor where the index is
(define (render2 edi)
(overlay/align  "left" "center" (beside (text (substring (editor2-text edi) 0 (editor2-index edi)) 11 "black") cursor (text (substring (editor2-text edi) (editor2-index edi) (string-length (editor2-text edi))) 11 "black")) (empty-scene 200 20)))

(define (edit2 ed ke)
  (cond
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(string=? "\b" ke) (if (= (editor2-index ed) 0) ed (make-editor2 (string-append (substring (editor2-text ed) 0 (- (editor2-index ed) 1)) (substring (editor2-text ed) (editor2-index ed) (string-length (editor2-text ed))))(- (editor2-index ed) 1)))]
    [(string=? "left" ke) (if (= (editor2-index ed) 0) ed (make-editor2 (editor2-text ed) (- (editor2-index ed) 1)))]
    [(string=? "right" ke) (if (= (editor2-index ed) (string-length (editor2-text ed))) ed (make-editor2 (editor2-text ed) (+ (editor2-index ed) 1)))]
    [(= (string-length ke) 1)(if (> (image-width (beside (text (substring (editor2-text ed) 0 (editor2-index ed)) 11 "black") ke (text (substring (editor2-text ed) (editor2-index ed) (string-length (editor2-text ed))) 11 "black"))) 200) ed (make-editor2 (string-append (substring (editor2-text ed) 0 (editor2-index ed)) ke (substring (editor2-text ed) (editor2-index ed) (string-length (editor2-text ed))))(+ (editor2-index ed) 1)))]
    [else ed]))
(define (run2 ed)
  (big-bang ed
    (on-key edit2)
    (to-draw render2)))