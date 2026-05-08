;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpret) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define (atom? at)
  (cond
    [(number? at) #true]
    [(symbol? at) #true]
    [else #false]))
; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define-struct function [name arg])
;define the name and the argument of a function
(make-function 'k (make-add 1 1))
(make-mul 5 (make-function 'k (make-add 1 1)))
(make-mul (make-function 'i 5)(make-function 'k (make-add 1 1)))


(make-add 10 -10)
(make-add (make-mul 20 3) 33)
(make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9)))

(define (eval-expression exp)
  (cond
    [(add? exp) (+ (eval-expression (add-left exp)) (eval-expression (add-right exp)))]
    [(mul? exp) (* (eval-expression (mul-left exp)) (eval-expression (mul-right exp)))]
    [(number? exp) exp]))

(define-struct et [left right])
(define-struct ou [left right])
(define-struct pas [exp])

(define (eval-bool-expression exp)
  (cond
    [(et? exp) (and (eval-expression (et-left exp)) (eval-expression (et-right exp)))]
    [(ou? exp) (or (eval-expression (ou-left exp)) (eval-expression (ou-right exp)))]
    [(pas? exp) (not (eval-expression (pas-exp exp)))]
    [(boolean? exp) exp]))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error "WRONG")])]
    [else (error "WRONG")]))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error "WRONG")]
    [(symbol? s) (error "WRONG")]))
 
; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

(define (interpreter-expr sexpr)
  (eval-expression (parse-sl sexpr)))

; BSL-var-expr + Symbol + Number -> BSL-var-expr
; replace all the symbol with the number
(define (subst ex x v)
  (local
    ((define (check-side lr)
       (cond
         [(struct? lr) (subst lr x v)]
         [(number? lr) lr]
         [(symbol=? x lr) v]
         [else lr])))
    (cond
      [(add? ex) (make-add (check-side (add-left ex)) (check-side (add-right ex)))]
      [(mul? ex) (make-mul (check-side (mul-left ex)) (check-side (mul-right ex)))])))

;BSL-var-expr -> boolean
;determine whether or not the bsl-var-expr still have symbol
(define (numeric? exp)
  (local
    ((define (check-side lr)
       (cond
         [(struct? lr) (numeric? lr)]
         [(number? lr) #true]
         [else #false])))
   (cond
     [(add? exp) (and (check-side (add-left exp)) (check-side (add-right exp)))]
     [(mul? exp) (and (check-side (mul-left exp)) (check-side (mul-right exp)))])))

(define (eval-variable exp)
  (cond
    [(numeric? exp) (eval-expression exp)]
    [else (error "not a BSL-exp")]))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(cons (cons 'a (cons 8 '())) '())

;BSL-var-exp + Association list -> value or error
;take the formula and applies the association list to it, then calculate the value
(define (eval-variable* ex da)
  (cond
    [(empty? da) (eval-variable ex)]
    [else (eval-variable* (subst ex (first (first da)) (second (first da))) (rest da))]))

(define exp1 (make-add (make-add 'x 5) (make-mul 4 'y)))
(define AL1 (list (list 'x 2) (list 'y 3)))
(check-expect (eval-variable* exp1 AL1) 19)

(define (eval-var-lookup e da)
  (cond
    [(number? e) e]
    [(symbol? e) (assq e da)]
    [(add? e) (+ (eval-var-lookup (add-left e) da) (eval-var-lookup (add-right e) da))]
    [(mul? e) (* (eval-var-lookup (mul-left e) da) (eval-var-lookup (mul-right e) da))]
    [(boolean? e) (error "variable non utilisée")]))

(define exemple1 (make-function 'f (make-mul 'x 3)))
(define exemple2 (make-mul 'y 4))
(define exemple3 (make-mul 4 5))
;BSL-fun-def symbol symbol BSL-fun-def
(define (eval-definition1 ex f x b)
     (cond
     [(number? ex) ex]
     [(function? ex)
      (if (symbol=? (function-name f))
      (local ((define value (eval-definition1 (function-arg ex) f x b))
         (define plugd (subst b x value)))
    (eval-definition1 plugd f x b)) (error "variable non utilisée"))]
     [(add? ex) (+ (eval-definition1 (add-left ex) f x b) (eval-definition1 (add-right ex) f x b))]
     [(mul? ex) (* (eval-definition1 (mul-left ex) f x b) (eval-definition1 (mul-right ex) f x b))]))

; Symbol + Symbol + BSL-fun-expression
;represent a function definition
(define-struct BSL-fun-def [name parameter body])

(define definition1 (make-BSL-fun-def 'f 'x (make-add 3 'x)))
(define g (make-BSL-fun-def 'g 'y (make-function 'f (make-mul 2 'y))))
(define definition3 (make-BSL-fun-def 'h 'v (make-add (make-function 'f 'v) (make-function 'g 'v))))

; BSL-fun-def* is either
; (cons BSL-fun-def BSL-fun-def*)
; '()

(define da-fgh (list definition1 g definition3))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(define (lookup-def da f)
  (cond
    [(empty? da) (error "aucune définition trouvée")]
    [(symbol=? (BSL-fun-def-name (first da)) f)(first da)]
    [else (lookup-def (rest da) f)]))

;BSL-fun-expr BSl-fun-def* -> result
(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(function? ex)
     (local ((define value (eval-function* (function-arg ex) da))
             (define looked-up-function (lookup-def da ex))
         (define plugd (subst (BSL-fun-def-body looked-up-function) (BSL-fun-def-parameter looked-up-function) value)))
    (eval-function* plugd da))]
    [(add? ex) (+ (eval-definition* (add-left ex) f x b) (eval-definition* (add-right ex) f x b))]
    [(mul? ex) (* (eval-definition* (mul-left ex) f x b) (eval-definition* (mul-right ex) f x b))]))

;BSL-da-all is a list
;it's either a
; (cons BSL-Fun-def BSL-da-all)
; (cons function BSL-da-all)
; '()

;BSL-da-all + function -> arg
(define (lookup-con-def db x)
  (cond
    [(empty? (first db)) (error "no such constant definition")]
    [(function? (first db)) (if (symbol=? (function-name (first db)) x) (first db) (lookup-con-def (rest db) x))]
    [else (lookup-con-def (rest db))]))

;BSL-da-all + function -> arg
(define (lookup-fun-def db x)
  (cond
    [(empty? (first db)) (error "no such constant definition")]
    [(BSL-fun-def? (first db)) (if (symbol=? (BSL-fun-def-name (first db)) x) (first db) (lookup-fun-def (rest db) x))]
    [else (lookup-fun-def (rest db))]))

(define (eval-all ex db)
  (cond
    [(number? ex) ex]
    [(boolean? ex) ex]
    [(function? ex)
     