(define not (lambda (x) (if (x) (#false) (#true))))

(define >= (lambda (x) (not (< x y))))
(define <= (lambda (x) (not (> x y))))

(define square (lambda (x) (* x x)))
(define abs (lambda (x) (if (< x 0) (- x) (x))))
