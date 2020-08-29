(in-package :cl-ifc)


(defstruct bnf-token
  (type)
  (data))

;; FALLBACK TOKENS, ORS ARE NESTED. no but for isntance in a list which in particular is it falling back from
;; Or literally make every list expansion a list of lists; normally its just one but |'s create another, that's how there's always room for one, and you don't need to know beforehand.
;; The | can literally just cons a new empty list onto the master list, and other tokens always add to the first list. 
(defun parse-rule (content &optional (closing-word nil) (or-statement nil))
  "Takes a list of bnf strings and returns a list of bnf tokens, nested as appropriate"
  (values 
   (remove nil
           (loop while (and content
                            (not (string= (first content) closing-word)))
                 collect
                 (let ((part (first content)))
                   (cond
                     ;; Or Statement : Needs to be caught first
                     ((and (not or-statement) (string= (second content) "|"))
                      (multiple-value-bind (tokens remaining-content)
                          (parse-rule content closing-word t)
                        (setf content remaining-content)
                        (make-bnf-token :type 'token-or
                                        :data tokens)))
                     ;; Literal string
                     ((uiop/utility:string-enclosed-p #\" part #\")
                      (pop content)
                      (make-bnf-token :type 'token-literal
                                      :data (strip-string part)))
                     ;; Group
                     ((string= part "(")
                      (multiple-value-bind (tokens remaining-content)
                          (parse-rule (rest content) ")")
                        (setf content (rest remaining-content))
                        (make-bnf-token :type 'token-group
                                        :data tokens)))
                     ;; Optional
                     ((string= part "[")
                      (multiple-value-bind (tokens remaining-content)
                          (parse-rule (rest content) "]")
                        (setf content (rest remaining-content))
                        (make-bnf-token :type 'token-optional
                                        :data tokens)))
                     ;; Repeating
                     ((string= part "{")
                      (multiple-value-bind (tokens remaining-content)
                          (parse-rule (rest content) "}")
                        (setf content (rest remaining-content))
                        (make-bnf-token :type 'token-repeating
                                        :data tokens)))
                     ;; Or (May not be good to just be a token, will see how the express parser handles it)
                     ((string= part "|")
                      (pop content)
                      nil)
                     ;; Default, rule expansion
                     (t
                      (pop content)
                      (make-bnf-token :type 'token-rule
                                      :data part))))))
   content))
             
                 
(defun parse-bnf (filename)
  "Loads a .bnf grammar file and returns a list of rules"
  (let ((lines (load-file-lines filename))
        (rules (make-hash-table :test #'equal)))
    (loop for line in lines do
          (let* ((parts (cl-utilities:split-sequence #\Space line))
                 (name (first parts))
                 (content (subseq parts 2 (- (length parts) 1))))
            (setf (gethash name rules)
                  (parse-rule content))))
    ;;(format t "~a~%" (print-hash-table rules))
    rules))
         
