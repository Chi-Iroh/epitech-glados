(define (prime? n)
  (if (< n 2)
      #f
      (let loop ((i 2))
        (cond
          ((> (* i i) n) #t) ; No divisors found
          ((= (remainder n i) 0) #f) ; Found a divisor
          (else (loop (+ i 1))))))) ; Check next potential divisor

(define (first-n-primes n)
  (define (find-primes count current primes)
    (if (= count n)
        (reverse primes)
        (if (prime? current)
            (find-primes (+ count 1) (+ current 1) (cons current primes))
            (find-primes count (+ current 1) primes))))
  (find-primes 0 2 '()))

(define (output-primes n)
  (for-each
   (lambda (p) (display p) (newline))
   (first-n-primes n)))

;; Output the first 10 primes
(output-primes 10)
