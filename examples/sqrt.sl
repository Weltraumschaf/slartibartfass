(define sqrt
    (lambda (x)
        (define good-enough?
            (lambda (guess)
                (< (abs (- (square guess) x)) 0.001)
            ))
        (define improve
            (lambda (guess)
                (average guess (/ x guess))
            ))
        (define average
            (lambda (a, b)
                (/ (+ a b) 2)
            ))
        (define sqrt-iter
            (lambda (guess)
                (if (good-enough? guess)
                    guess
                    (sqrt-iter (improve guess)))
            ))
        (sqrt-iter 1.0)
    ))

