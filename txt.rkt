;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname txt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

(read-words/line "ttt.txt")

; A list of string is one of
; '()
; (cons String List-of-string)

; A list of List of string is one of
; List of string
; (cons String List-of-List-of-string)
; (cons List-of-list-of-string List-of-list-of-String)

; LN -> List-of-numbers
; determines the number of words on each line 
; An LN is one of: 
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings
 
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))
 
; LN -> List-of-numbers
; determines the number of words on each line 
 
(check-expect (words-on-line ln0) '())
(check-expect (words-on-line ln1) (cons 2 (cons 0 '())))
 
(define (words-on-line ln)
  (cond
    [(empty? ln) '()]
    [else (cons (length (first ln))
                (words-on-line (rest ln)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

; prend une liste de line
; sort un string où les différents string des lignes sont séparés par des espace et les lignes par des \n
(define (collapse c)
  (cond
    [(empty? (rest c)) (final-collapse c)]
    [(empty? (first c)) (string-append "\n" (collapse (rest c)))]
    [(string? (first c)) (string-append (first c) " " (collapse (rest c)))]
    [(cons? (first c))(string-append (collapse (first c))(collapse (rest c)))]))


(define (final-collapse c)
  (cond
    [(empty? (first c))  "\n"]
    [(string? (first c)) (first c)]
    [(cons? (first c)) (collapse (first c))]))
(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))

; nom d'un fichier -> fichier
; prend un fichier d'après son nom et crée un fichier identique avec no-articles- en préfixe et sans a an et the
(define (no-article f)
  (write-file (string-append "no-articles-" f)
      (collapse (article-suppresor (read-words/line "ttt.txt")))))

(define (article-suppresor ll)
  (cond
    [(empty? ll) '()]
    [else
     (cons (art# (first ll)); a list of strings
      (article-suppresor (rest ll)))]))

(define (art# ll)
 (cond
   [(empty? ll) '()]
   [(or (string=? (first ll) "a")(string=? (first ll) "an")(string=? (first ll) "the")) (cons '() (art# (rest ll)))]
   [else (cons (first ll) (art# (rest ll)))]))

(define (encoding f)
  (write-file (string-append "encoded-" f)
       (encode (explode (collapse (read-words/line f))))))

(define (encode l)
  (cond
    [(empty? l) ""]
    [else (string-append (encode-letter (first l)) (encode (rest l)))]))

(define-struct resultatwc [1strin words lines])

(define (wc f)
  (make-resultatwc (length (read-1strings f)) (length (read-words f)) (length (read-lines f))))

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
 

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

; 11 12
; 21 22

; 11 21
; 12 22

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
 
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else
     (cons (first l) (add-at-end (rest l) s))]))

(define (create-editor pre post)
  (make-editor (rev pre) post))

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e)
  (place-image/align
    (beside (editor-text reverse (editor-pre e))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))

(define (editor-text s)
  (text (implode-fm s) FONT-SIZE FONT-COLOR))

(define (implode-fm s)
  (cond
    [(empty? s) ""]
    [else (string-append (first s) (implode-fm (rest s)))]))
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; insert the 1String k between pre and post
(define (editor-ins ed )
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))

(check-expect (editor-lft (make-editor (cons "c" (cons "b" (cons "a" cons '()))) (cons "d" (cons "e" (cons "f" cons '()))))) (make-editor (cons "b" (cons "a" cons '())) (cons "c" (cons "d" (cons "e" (cons "f" '()))))))
(check-expect (editor-lft (make-editor '() (cons a (cons b (cons c '()))))) (make-editor '() (cons a (cons b (cons c '())))))

(define (editor-lft ed)
  (cond
    [(empty? (first (editor-pre ed))) ed]
    [else (make-editor (rest (editor-pre ed)) (add-at-end (editor-post ed) (first (editor-pre ed))))]))

(check-expect (editor-rgt (make-editor (cons "c" (cons "b" (cons "a" cons '()))) (cons "d" (cons "e" (cons "f" cons '()))))) (make-editor (cons "d"(cons "c"(cons "b" (cons "a" cons '())))) (cons "e" (cons "f" ))))

(define (editor-rgt ed)
  (cond
    [(empty? (rest (editor-post ed))) ed]
    [else (make-editor (cons (search-last (editor-post ed ed)) (editor-pre ed)) (remove-last (editor-post ed))]))
(check-expect (editor-del (make-editor '() (cons a (cons b '())))) (make-editor '() (cons a (cons b '()))))
(check-expect (editor-del (make-editor (cons c (cons b (cons a '()))) (cons d (cons e (cons f '()))))) (make-editor (cons b (cons a '())) (cons d (cons e (cons f '())))))
(define (editor-del ed)
  (cond
    [(empty? (first (editor-pre ed))) ed]
    [else (make-editor (rest (editor-pre ed)) (editor-post ed))]
; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

(define (search-last l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (search-last (rest l))]))

(define (remove-last l)
  (cond
    [(empty? (rest l)) '()]
    [else (cons (first l) (remove-last (rest l)))]))

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))