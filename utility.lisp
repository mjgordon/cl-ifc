(in-package :cl-ifc-gen)

(defparameter *debug-level* 0)


;;; Control Flow

(defmacro if-let ((var test-form) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form ,else-form)))


(defmacro multiple-value-if (test values-list then else)
  "Combines a multiple-value-bind and if statement, with the assumption that the first returned value of the
test function be applied to the if"
  (let ((match-p (gensym)))
    `(multiple-value-bind ,(cons match-p values-list) ,test
       (if ,match-p
           ,then
           ,else))))


;;; File IO

(defun load-file-lines (filename)
  "Reads a file and returns a list containing a string for each line"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))


;;; Packages

(defun list-package (package)
  "Return all symbols defined in a package"
  (loop for s being the symbols of (find-package package)
        collect s))


;;; Printing

(defun print-chars (c amt)
  "Print character 'c' 'amt' number of times"
  (format t "~v@{~A~:*~}" amt c))


(defun print-debug (message level &optional (indent 0))
  "If level is in the range of the *debug-level*, print the message with 'indent' pipe characters before"
  (when (>= *debug-level* level)
    (print-chars #\| indent)
    (format t "~a~%" message)))


(defun print-hash-table (table)
  "Show all entries in a hash table"
  (loop for key being each hash-key of table using (hash-value value)
        collect (list key value)))


;;; String Operations

(defun char-list (string)
  "Converts an array of characters (string) to a list of characters"
  (loop for char across string collect char))


(defmacro char-list-literal (string-literal)
  "Macro for using character list literals at compile time"
  `',(loop for char across string-literal collect char))


(defun lines-to-characters (lines)
  "Returns a flat list of characters from a list of strings"
  (apply #'concatenate 'list
         (loop for line in lines collect
               (loop for char across line collect char))))


(defun string-from-char-list (characters)
  (format nil "~{~A~}" characters))


(defun strip-sequence (string)
  "Removes the first and last characters of a string (useful if quoted etc)"
  (subseq string 1 (- (length string) 1)))


;;; Variables

(defmacro set-temp (location value &body body)
  "Create a new binding for variable 'location' in a local scope"
  (let ((temp-storage (gensym)))
    `(let ((,temp-storage ,location))
       (setf ,location ,value)
       ,@body
       (setf ,location ,temp-storage))))
