;; [1 2 (+ 3 4) 5] syntax

(defun rb-vector-reader (stream char)
  (declare (ignore char))
  `(rb-vector:rb-vector ,@(read-delimited-list #\] stream t)))

(set-macro-character #\[ 'rb-vector-reader)
(set-macro-character #\] (get-macro-character #\) nil))
