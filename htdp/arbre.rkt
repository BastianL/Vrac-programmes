;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname arbre) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)

(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))


; An FF (short for family forest) is one of: 
; – '()
; – (cons FT FF)
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

; List-of
;-'()
;- (cons family-tree [List-of family-tree])
(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; FF -> Boolean
; does the forest contain any child with "blue" eyes
 
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)

(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
 
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

(define (blue-eyed-child-in-forest? a-forest)
  (cond
    [(empty? a-forest) #false]
    [else
     (or (blue-eyed-child? (first a-forest))
         (blue-eyed-child-in-forest? (rest a-forest)))]))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          

; An Atom is one of: 
; – Number
; – String
; – Symbol

(define (atom? data)
  (cond
    [(or (number? data) (string? data) (symbol? data)) #true]
    [else #false]))

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
 (local
   ((define (count-sl sl)
  (cond
    [(empty? sl) 0]
    [else
     (+ (count (first sl) sy) (count-sl (rest sl)))]))
    ; Atom Symbol -> N 
    ; counts all occurrences of sy in at 
   (define (count-atom at)
     (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)])))
 
 (cond
   [(atom? sexp) (count-atom sexp)]
   [else (count-sl sexp)])))

(define (depth sexp)
  (local
    ((define (depth-sl)
       (cond
         [(empty? sl) 0]
         [else (max (add1 (depth (first sl))) (depth-sl (rest sl)))])))
  (cond
    [(atom? sexp) 1]
    [else (depth-sl sexp)])))

(define (substitute s old new)
  (local
    ((define (substitute-sl s)
       (cond
         [(empty? sl) '()]
         [else
          (cons (substitute (first sl)) (substitute-sl (rest sl)))]))
     (define (substitue-atom at)
       (cond
         [(and (number? at) (number? old))(if (= at old) new at)]
         [(and (string? at) (string? old))(if (string=? at old) new at)]
         [(and (symbol? at) (symbol? old))(if (symbol=? at old) new at)]
         [else old])))
   (cond
     [(atom? s) (substitue-atom s)]
     [else (substitute-sl s)])))

