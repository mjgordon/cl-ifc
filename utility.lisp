(in-package :cl-ifc-gen)

(defmacro set-temp (location value &body body)
  (let ((temp-storage (gensym)))
    `(let ((,temp-storage ,location))
       (setf ,location ,value)
       ,@body
       (setf ,location ,temp-storage))))

;;; File IO

(defun load-file-lines (filename)
  "Reads a file and returns a list containing a string for each line"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))


;;; Printing

(defun print-hash-table (table)
  (loop for key being each hash-key of table using (hash-value value)
        collect (list key value)))


;;; String Operations

(defun strip-string (string)
  "Removes the first and last characters of a string (useful if quoted etc)"
  (subseq string 1 (- (length string) 1)))
