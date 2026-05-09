;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname gaussian) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

(define (check-solution soe solution)
  (cond
    [(empty? soe) #true]
    [(check-line (first soe) solution) (check-solution (rest soe) solution)]
    [else #false]))

(define (check-line eq solution)
  (if (= (check-number (lhs eq) solution) (rhs eq)) #true #false))

(define (check-number lhseq solution)
  (cond
    [(empty? lhseq) 0]
    [(> (length solution) (length lhseq)) (check-number lhseq (rest solution))]
    [else (+ (* (first lhseq) (first solution)) (check-number (rest lhseq) (rest solution)))]))

(define (substract eq1 eq2)
  (local
    ((define sub-coef (/ (first eq1) (first eq2)))
     (define (sub-equ eq1 eq2)
       (cond
       [(empty? eq1) '()]
        [else (cons (- (first eq1) (* (first eq2) sub-coef)) (sub-equ (rest eq1) (rest eq2)))])))
    (rest (sub-equ eq1 eq2))))

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

(define (rt L) (append (rest L) (list (first L))))

; SOE -> TM
; triangulates the given system of equations 
(define (triangulate M)
  (cond
    [(empty? M) '()]
    [(= (first (first M)) 0)
     (local
       ((define (check-triangulation M)
          (cond
            [(empty? M) #false]
            [(= (first (first M)) 0) (check-triangulation (rest M))]
            [else #true])))
       (if (check-triangulation M) (triangulate (rt M)) (error "tout les coefficients commencent par zero")))]
    [else
     (local
       ((define (sub-triangulate Eq1 Mr)
          (cond
            [(empty? Mr) '()]
            [(= (length Eq1) (length (first Mr))) (cons (substract (first Mr) Eq1) (sub-triangulate Eq1 (rest Mr)))]
            [else (sub-triangulate Eq1 (rest Mr))])))
    (cons (first M) (triangulate (sub-triangulate (first M) (rest M)))))]))


;System of equation -> solution
;consomme un système d'equation, substitue un a un les nombres pour obtenir une solution.
(define (solve SoeT)
  (reverse (solve2 (reverse SoeT) '())))

(define (solve2 rSoeT sol)
  (cond
    [(empty? rSoeT) '()]
    [else
     (local
       ((define (replace lhs1 sol)
          (cond
            [(empty? lhs1) '()]
            [(> (length lhs1) (length sol)) (cons (first lhs1)(replace (rest lhs1) sol))]
            [else (cons (* (first lhs1) (first sol)) (replace (rest lhs1) (rest sol)))]))
        (define (solve-line lhs1 rhs1)
          (/(- rhs1 (foldr + 0 (rest lhs1))) (first lhs1)))
        (define solved-line (solve-line (replace (lhs (first rSoeT)) sol) (rhs (first rSoeT)))))
         (cons solved-line (solve2 (rest rSoeT) (cons solved-line sol))))]))


(define (gauss M)
  (solve (triangulate M)))