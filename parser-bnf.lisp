(in-package :cl-ifc)


(defstruct bnf-token
  (type)
  (data))


(defun parse-rule (content &optional (closing-word nil))
  "Takes a list of bnf strings and returns a list of bnf tokens, nested as appropriate"
  (values 
   (loop while (and content
                    (not (string= (first content) closing-word)))
         collect
         (let ((part (pop content)))
           (cond
             ;; Literal string
             ((uiop/utility:string-enclosed-p #\" part #\")
              (make-bnf-token :type 'token-literal
                              :data (strip-string part)))
             ;; Group
             ((string= part "(")
              (multiple-value-bind (tokens remaining-content)
                  (parse-rule content ")")
                (setf content (rest remaining-content))
                (make-bnf-token :type 'token-group
                                :data tokens)))
             ;; Optional
             ((string= part "[")
              (multiple-value-bind (tokens remaining-content)
                  (parse-rule content "]")
                (setf content (rest remaining-content))
                (make-bnf-token :type 'token-optional
                                :data tokens)))
             ;; Repeating
             ((string= part "{")
              (multiple-value-bind (tokens remaining-content)
                  (parse-rule content "}")
                (setf content (rest remaining-content))
                (make-bnf-token :type 'token-repeating
                                :data tokens)))
             ;; Or (May not be good to just be a token, will see how the express parser handles it)
             ((string= part "|")
              (make-bnf-token :type 'token-or
                              :data nil))
             ;; Default, rule expansion
             (t
              (make-bnf-token :type 'token-rule
                              :data part)))))
   content))
             
                 
(defun parse-bnf (filename)
  "Loads a .bnf grammar file and returns a list of rules"
  (let ((lines (load-file-lines filename))
        (rules (make-hash-table)))
    (loop for line in lines do
          (let* ((parts (cl-utilities:split-sequence #\Space line))
                 (name (first parts))
                 (content (subseq parts 2 (- (length parts) 1))))
            (setf (gethash name rules)
                  (parse-rule content))))
    ;;(format t "~a~%" (print-hash-table rules))
    rules))
         
