(in-package :cl-ifc)

(defun word-end-syntactic-p (char)
  (member char '(#\Return
                 #\Linefeed
                 #\Space
                 #\Tab)))

(defun word-end-semantic-p (char)
  (member char '(#\;
                 #\,)))

(defun consume-until-close-comment (input)
  (loop while input do
        (let ((word (pop input)))
          (when (string= word "*)")
            (return input)))))

(defun process-schema (schema-words &optional (elements (list)))
  (when schema-words
    (let ((word (pop schema-words)))
      (cond ((string= word "(*") (process-schema (consume-until-close-comment schema-words) elements))))))


(defun lines-to-words (lines)
  "Converts a list of strings to a flat list of words"
  (let ((words (list))
        (current-word (make-array 0
                                  :element-type 'character
                                  :fill-pointer 0
                                  :adjustable t)))
    (loop for line in lines do
          (loop for char across line do
                (if (or (word-end-syntactic-p char) (word-end-semantic-p char))
                    (when (string/= "" current-word)
                      (progn
                        (setf words (cons current-word words))
                        (when (word-end-semantic-p char) ; ';' and ',' characters are syntactic
                          (setf words (cons (string char) words)))
                        (setf current-word (make-array 0
                                                       :element-type 'character
                                                       :fill-pointer 0
                                                       :adjustable t))))
                    
                    (vector-push-extend char current-word))))
    (setf words (cons current-word words))
    (setf words (reverse words))
    (loop for word in words do
          (format t "~a~%" word))))

(defun parse-main (input output)
  (let ((word (pop input)))
    (when (string= word "(*")
      (parse-main (consume-until-close-comment input) output))))
  


(defun parse-express (filename)
  (let* ((lines (load-file-lines filename))
         (words (lines-to-words lines)))
    
    ))


