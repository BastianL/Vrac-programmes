;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname interpreting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

(make-add 10 -10)
(make-add (make-mul 20 3) 33)
(make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9)))

(define (eval-expression expr)
  (cond
    [(number? expr) expr]
    [(add? expr) (+ (eval-expression (add-left expr)) (eval-expression (add-right expr)))]
    [(mul? expr) (* (eval-expression (mul-left expr)) (eval-expression (mul-right expr)))]))

(define-struct et [left right])
(define-struct ou [left right])
(define-struct pas [arg])
(define (eval-bool-expression expr)
  (cond
    [(boolean? expr) expr]
    [(et? expr) (and (eval-bool-expression (et-left expr)) (eval-bool-expression (et-right expr)))]
    [(ou? expr) (or (eval-bool-expression (ou-left expr)) (eval-bool-expression (ou-right expr)))]
    [(pas? expr) (not (eval-bool-expression (pas-arg expr)))]))

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
       [else (error WRONG)])]
    [else (error WRONG)]))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
 
; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))