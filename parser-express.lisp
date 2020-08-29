(in-package :cl-ifc)

(defparameter *bnf-dict* nil)

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
    (setf words (reverse words))))

(defun parse-main (grammar input output)
  (let ((grammar-head (first grammar)))
    (format t "~a~%" grammar-head)
    (cond
      ;; Head is a rule
      ((eq (bnf-token-type grammar-head) 'token-rule)
       (parse-main (gethash (bnf-token-data grammar-head) *bnf-dict*) input output))
      ;; Head is a literal
      ((eq (bnf-token-type grammar-head) 'token-literal)
       (if (equal (bnf-token-data grammar-head) (first input))
           (if (null grammar)
               (values input output)
               (parse-main (rest grammar) input output))
           (values input nil)))
      ;; 
      ((eq )))))
  

  


(defun parse-express (filename)
  (let* ((lines (load-file-lines filename))
         (words (lines-to-words lines)))
    (setf *bnf-dict* (cl-ifc:parse-bnf (asdf:system-relative-pathname :cl-ifc "schemas/express.bnf")))
    (format t "BNF Rule Count : ~a~%" (hash-table-size *bnf-dict*))
    (format t "Express Word Count : ~a~%" (length words))
    (parse-main (gethash "syntax" *bnf-dict*) words (list))
    nil))


