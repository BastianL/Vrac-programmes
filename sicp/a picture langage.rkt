#lang sicp
(#%require sicp-pict)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define zorro mark-of-zorro)

(define (split op1 op2)
   (define (split-recursive painter n)
       (if (= n 0)
           painter
           (let ((smaller (split-recursive painter (- n 1))))
             (op1 painter (op2 smaller smaller)))))
  (lambda (painter n) (split-recursive painter n)))

(define right-split (split beside below))

(define up-split (split below beside))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


(define (xcor-vect vect)
  (vector-xcor vect))

(define (ycor-vect vect)
  (vector-ycor vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (ycor-vect vect2)) (+ (xcor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
    (make-vect (- (xcor-vect vect1) (ycor-vect vect2)) (- (xcor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scalar vect)
  (make-vect (* (xcor-vect vect) scalar) (* (ycor-vect vect) scalar)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


(define (outline frame)
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 0) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1)))))

(define (xpicture frame)
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                          (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define (diamond frame)
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))


(define basic-frame (make-frame (make-vect 0 0) (make-vect 0 1) (make-vect 1 0)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))



(define squashed-einstein (squash-inwards einstein))

(paint (squashed-einstein basic-frame))
