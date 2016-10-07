(define (not x)
        (if (= #true x)
            #false
            #true))

(define (>= x)
        (not (< x y)))

(define (<= x)
        (not (> x y)))

(define (square x)
        (* x x))

(define (cube x)
        (* x x x))

(define (abs x)
        (if (< x 0)
            (- x)
            x))
