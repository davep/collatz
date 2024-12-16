(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))

(declaim
 (ftype (function (fixnum) fixnum) collatz)
 (inline collatz))
(defun collatz (n)
  "Calculate the next entry in the Collatz sequence for N."
  (declare (fixnum n))
  (if (zerop (mod n 2))
      (/ n 2)
      (1+ (* n 3))))

(defun collatz-sequence (n)
  "Given N, calculate the whole Collatz sequence until we reach 1."
  (declare (fixnum n))
  (let ((next (collatz n)))
    (cons n
          (funcall
           (if (eq next 1)
               #'list
               #'collatz-sequence)
           next))))

(defun collatz-sizes (n)
  "Calculate the sizes of every sequence from 1 to N."
  (declare (fixnum n))
  (loop for i from 1 to n collect (cons i (length (collatz-sequence i)))))

(defun longest-sequence-from-1-to (n)
  "Find which longer has the longest Collatz between 1 and N.

The return values are the number that generates the sequence and the length
of the sequence."
  (declare (fixnum n))
  (let ((longest (car (sort (collatz-sizes n) #'> :key #'cdr))))
    (values (car longest) (cdr longest))))

;;; collatz.lisp ends here
