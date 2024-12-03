(define (sierpinski size)
  (define (draw-row row)
    (for-each display row)
    (newline))

  (define (make-next-row prev-row)
    (let ((n (length prev-row)))
      (map (lambda (i)
             (if (or (= i 0) (= i (- n 1)))
                 #\space
                 (if (and (char=? (list-ref prev-row (- i 1)) #\#)
                          (char=? (list-ref prev-row (+ i 1)) #\#))
                     #\space
                     #\#)))
           (iota n))))

  (define (generate-rows size)
    (let loop ((rows '((#\#))))
      (if (= (length rows) size)
          (reverse rows)
          (let* ((prev-row (car rows))
                 (next-row (make-next-row (cons #\space (append prev-row '(#\space))))))
            (loop (cons next-row rows))))))

  (for-each draw-row (generate-rows size)))

;; Call the function with desired size
(sierpinski 16)

