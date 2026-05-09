;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname word-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(define def-dic (read-lines "samplewords.txt"))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))
 
; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))(words->strings (rest low)))]))
(check-expect (words->strings (list (list "b""a""s")(list"t""i""a""n")))(list "bas" "tian"))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(search-word (first los) def-dic )(cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))

(define (search-word stri dic)
  (cond
  [(empty? dic) #false]
  [(string=? stri (first dic))#true]
  [else (search-word stri (rest dic))]))

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

(define word1 (list "b" "a" "s" "t" "i" "a" "n"))
(define word2 (list "l" "i" "s" "r" "i" "n"))
(define word3 (list "o" "f"))
(define word4 (list "a"))

; A List-of-words is one of:
; - '()
; - (cons Word List-of-words
; interpretation a List-of-words is a succession of words.
(define low1 (list word1 word2))
(define low2 (list word4))
(define low3 '())
; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

;; insert-everywhere/in-all-words : symbol list-of-words -> list-of-words
;; produces a list of words like the list of words given, but with the 
;; given letter inserted between all letters and at the beginning and end
;; of each word in the word list given 

(define (insert-everywhere/in-all-words l alow)
  (cond
    [(empty? alow) empty]
    [else (append (insert-everywhere l (first alow))
                  (insert-everywhere/in-all-words l (rest alow)))]))

;; insert-everywhere : symbol word -> list-of-words
;; given l and a-word, produces a list of words like a-word, but with
;; l insert at the beginning, between all possible letters, or at the end

(define (insert-everywhere l a-word)
  (cond
    [(empty? a-word) (cons (cons l a-word) empty)]
    [else
     (cons (cons l a-word)
           (prefix-all (first a-word)
                       (insert-everywhere l (rest a-word))))]))

;; prefix-all : symbol list-of-words -> list-of-words
;; given a symbol and a list of words, produces a new list of words
;; like that given, but with symbol l prefixed to each

(define (prefix-all l words) 
  (cond
    [(empty? words) empty]
    [else (cons (cons l (first words))
                (prefix-all l (rest words)))]))  

; String -> Word
; converts s to the chosen word representation 
(define (string->word s) (explode s))
 
; Word -> String
; converts w to a string
(define (word->string w) (implode w))