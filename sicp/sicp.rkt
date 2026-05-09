#lang sicp
(define (sum term a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product term a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (recursproduct term a next b)
  (if (> a b)
      1
      (* (term a)
         (sum term (next a) next b))))
(define (nochange x) x)
(define (carre x) (* x x))
(define (factorielle a) (product nochange 1 inc a))
(define (pi-formula n)
  (define o (* n 2))
  (/(* o (+ 2 o)) (carre (+ o 1))))

(define (pi-product n)
  (*(product pi-formula 1 inc n) 4))

(define (accumulateiterate combiner null-value term a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulaterecursive combiner null-value term a next b)
   (if (> a b)
      null-value
      (combiner (term a)
         (accumulaterecursive term (next a) next b))))

(define (prime? n)
  (define (prime-test x)
    (if (> x (sqrt n)) #t (if (= (modulo n x) 0) #f (prime-test (inc x)))))
  (prime-test 2))

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (combiner (if (filter a) (term a) null-value) result))))
  (iter a null-value))

(define (gcd i n)
  (define (gcalculd x)
    (cond
      ((< x 2) x)
      ((and (= (modulo i x) 0) (= (modulo n x) 0)) x)
          (else gcalculd (dec x))))
  (gcalculd (- i 1)))
(define (prime-sum a b)
  (filtered-accumulate prime? + 0 carre a inc b))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y) (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;(define gn
  ;(fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 0.5))

;(define xx (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))

(define (cont-frac n d k)
  (define (cont-fraci i)
    (if (>= i k)
        (+ (d (- i 1)) (/ (n i) (d i)))
        (+ (d (- i 1)) (/ (n i) (cont-fraci (inc i))))))
  (/ (n 1) (cont-fraci 2)))


(define (iteracont-frac n d k)
  (define (iter x result) 
     (if (= x 0) 
         result 
         (iter (- x 1) (/ (n x) (+ (d x) result))))) 
   (iter k 0))
"carre d'or"
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x))) (lambda (i) (+ i (- i 1) )) k))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (compose f f))

(define (repeated f n)
  (if (> n 1) (compose f (repeated f (- 1 n))) f))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x 0.01)) (f x) (f (+ x 0.01))) 3)))

(define (nfold f n)
  ((repeated smooth n) f))

 (define (iterative-improve good-enough? improve)  
   (define (iter guess)  
     (if (good-enough? guess)  
         guess  
         (iter (improve guess))))  
   iter) 
