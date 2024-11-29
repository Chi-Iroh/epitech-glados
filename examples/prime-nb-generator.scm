(defun is-prime (n)
  "Check if a number N is prime."
  (cond
    ((< n 2) NIL)
    ((= n 2) T)
    (t (loop for i from 2 to (isqrt n)
             never (zerop (mod n i))))))

(defun generate-primes (n)
  "Generate a list of the first N prime numbers."
  (let ((primes '()) ; Initialize an empty list to store primes
        (candidate 2)) ; Start checking from 2
    (loop while (< (length primes) n)
          do (when (is-prime candidate)
               (push candidate primes))
          (incf candidate))
    (reverse primes))) ; Reverse to maintain order

(generate-primes 10)