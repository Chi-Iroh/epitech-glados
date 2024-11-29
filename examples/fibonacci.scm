(defun fibonacci (n)
  (loop for i from 0 below n
        collect (if (< i 2) i (+ (nth (- i 1) (fibonacci (- n 1)))
                                 (nth (- i 2) (fibonacci (- n 1)))))))

(fibonacci 8)
