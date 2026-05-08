;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname symbol) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(check-expect (replace-eol-with '() '(a b)) '(a b))

(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
                (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))

(define (cross ls ln)
 (for*/list ((i ls) (j ln))
   (list i j)))

; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length 
(define (wages*.v2 hours wages/h)
  '())

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons
       (weekly-wage (first hours) (first wages/h))
       (wages*.v2 (rest hours) (rest wages/h)))]))
; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

(define-struct employee [name ssn payrate])
(define-struct work-record [name hourworked])
(define-struct result [name wage])
(define (wages*.v3 employee work-record)
  (local ((define (find-work-record employname wrlist)
            (cond
              [(string=? employname (work-record name(first wrlist))) (first wrlist)]
              [else (find-work-record employname (rest wrlist))])))
  (cond
   [(empty? name) '()]
   [else (cons (weekly-wage.v2 (first employee) (find-work-record (employee-name (first employee)) work-record))
             (wages*.v3 (rest employee) work-record))])))

(define (weekly-wage.v2 e wr)
  (list (employee-name e) (* (work-record-hourworked wr) (employee-payrate e))))

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

(define zip (ln lp)
  (cond
    [(empty? ln) '()]
    [else (cons (make-phone-record (first ln) (first lp)) (zip (rest ln) (rest lp)))]))

; N is one of: 
; – 0
; – (add1 N)

; list-pick: [List-of Symbol] N[>= 0] -> Symbol
; determines the nth symbol from alos, counting from 0;
; signals an error if there is no nth symbol
(define (list-pick alos n)
  (cond
    [(empty? alos) (error 'list-pick "list too short")]
    [(= n 0) (first alos)]
    [(> n 0) (list-pick (rest alos) (sub1 n))]))

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)

(define-struct branch [left right])

; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 

(define (tree-pick ts ld)
  (cond
    [(empty? ld) ts]
    [(symbol=? 'left (first ld)) (tree-pick (branch-left ts) (rest ld))]
    [(symbol=? 'right (first ld)) (tree-pick (branch-right ts) (rest ld))]))

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R
  

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

(define (intersect s1 s2)
  (local ((define (check-union n s)
    (cond
      [(empty? s) '()]
      [(= n (first s)) (cons n (check-union n (rest s)))]
      [else (check-union (rest s))])))
    (cond
      [(empty? s1) '()]
      [else (append (check-union (first s1) s2) (union (rest s1 s2)))])))

(define (union s1 s2)
  (append s1 s2))

(define list1 (list 1 3 5 7 7 9))
(define list2 (list 2 3 6 9 10 10 14 

(define (merge l1 l2)
  (cond
    [(and (empty? l1) (empty? l2))'()]
    [(empty? l1) (cons (first l2) (merge l1 (rest l2)))]
    [(empty? l2) (cons (first l1) (merge (rest l1) l2))]
    [(> (first l1) (first l2)) (cons (first l2) (merge l1 (rest l2)))]
    [(<= (first l1) (first l2)) (cons (first l1) (merge (rest l1) l2))]))

(define (take l n)
  (cond
    [(or (empty? l) (= 0 n)) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

(define (drop l n)
  (cond
    [(empty? l) '()]
    [(= 0 n) l]
    [else (drop (rest l) (sub1 n))]))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status))
          (define (compare-word to-guess s ke)
            (cond
              [(empty? to-guess) '()]
              [(string=? (first to-guess) ke) (cons ke (compare-word (rest to-guess) (rest s) ke))]
              [else (cons (first s) (compare-word (rest to-guess) (rest s) ke))]))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare])))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

(define-struct ETC [number hours])
(define-struct ER [name number payrate])
(define-struct wage-record [name wage])

(define (wages*.v3 lister listtc)
  (local
    ((define (wagev3 er listtc)
       (cond
         [(empty? listtc) (error "time-card record non trouvée")]
         [(= (ER-number er) (ETC-number (first listtc))) (make-wage-record (ER-name er) (* (ER-payrate er) (ETC-hours (first listtc))))]
         [else (wagev3 er (rest listtc))])))
    (cond
      [(empty? lister) '()]
      [else (cons (wagev3 (first lister) listtc) (wages*.v3 (rest lister) listtc))])))

(define (value lc lv)
  (cond
    [(empty? lc) 0]
    [else (+ (* (first lv) (expt (first lc) (length lc))) (value (rest lc) (rest lv)))]))

(define listname (list "Louise" "Jane" "Laura" "Dana" "Mary"))

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))


; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
; see exercise 213
(define (arrangements names)
  (cond
    [(empty? names) (list '())]
    [else (insert-everywhere/in-all-words (first names)
            (arrangements (rest names)))]))

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

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (list-ref l (random (length l))))
 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place 
(define (non-same names ll)
  (cond
    [(empty? (first ll)) '()]
    [(is-non-same (first ll) names) (cons (first ll) (non-same names (rest ll)))]
    [else (non-same names (rest ll))]))

(define (is-non-same l1 l2)
  (cond
    [(empty? l1) #true]
    [(string=? (first l1) (first l2)) #false]
    [else (is-non-same (rest l1) (rest l2))]))

(define (DNAprefix pattern search-string)
 (DNA pattern search-string #true))

(define (DNAdelta pattern search-string)
  (DNA pattern search-string (if (empty? search-string) (error " search-string trop réduite")(first search-string))))

(define (DNA pattern search-string reaction)
  (cond
    [(empty? pattern) reaction]
    [(symbol=? (first pattern) (first search-string)) (DNA (rest pattern) (rest search-string))]
    [else #false]))
; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

(define (sexp=? s1 s2)
  (cond
    [(empty? s1) (if (empty? s2) #true #false)]
    [(number? s1) (if (number? s2) (if (= s1 s2) (sexp=? (rest s1) (rest s2)) #false) #false)]
    [(string? s1) (if (string? s2) (if (string=? s1 s2) (sexp=? (rest s1) (rest s2)) #false) #false)]
    [(string? s1) (if (symbol? s2) (if (symbol=? s1 s2) (sexp=? (rest s1) (rest s2)) #false) #false)]))