(in-package :cl-ifc)


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
  (subseq string 1 (- (length string) 1)))
