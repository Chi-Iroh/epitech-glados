(defun sierpinski (depth)
  "Generate an ASCII Sierpiński Triangle of the given DEPTH."
  (let ((triangle (make-array (expt 2 depth) :initial-element '())))
    (labels ((draw (row col size)
               (if (= size 1)
                   (setf (aref triangle row) (cons col (aref triangle row)))
                   (let ((half (/ size 2)))
                     (draw row col half)
                     (draw (+ row half) col half)
                     (draw (+ row half) (+ col half) half)))))
      (draw 0 0 (expt 2 depth))
      (loop for row from 0 below (length triangle) do
            (format t "~v@{~a~}" (- (length triangle) row)
                    (mapcar (lambda (col)
                              (if (member col (aref triangle row))
                                  "*"
                                  " "))
                            (loop for i from 0 below (expt 2 depth) collect i))))
      (format t "~%"))))

(sierpinski 4)
