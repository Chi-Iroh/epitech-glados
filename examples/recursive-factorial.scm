(define (factorial x)
    (if (eq? x 1)
        1
        (* x (factorial (- x 1)))
    )
)
(factorial 10)