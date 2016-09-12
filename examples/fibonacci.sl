(define fib
    (lambda (n)
        (if (< n 2)
            1
            (+ (fib (- n 1))
               (fib (- n 2))))))

(define loop (lambda (i) 
    ))
               
(print (quote fib 10 = ))
(println (fib 10))
(print (quote fib 11 = ))
(println (fib 11))
(print (quote fib 12 = ))
(println (fib 12))
