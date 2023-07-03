(in-package :cl-ifc)


(defstruct bnf-token
  (type)
  (data))


(defun is-or (item)
  (and (eq (type-of item) 'bnf-token)
       (eq (bnf-token-type item) 'token-or)))


(defun output-add (output item)
  "If the output currently being built is an options token, add to the internal data list. Otherwise, add to the raw output list"
  (progn 
    (if (is-or (first output))
        (setf (bnf-token-data (first (bnf-token-data (first output))))
              (cons item (bnf-token-data (first (bnf-token-data (first output))))))
        (setf output (cons item output)))
    output))


(defun parse-bnf-continue (input output closer)
  (if (or (null (rest input))
          (string= (second input) closer))
      (progn
        (if (is-or (first output))
            (setf (bnf-token-data (first output)) (reverse (bnf-token-data (first output))))
            (setf output (reverse output)))
        
        (values (cdr input) output))
      (parse-bnf-rule (rest input) output closer)))


(defun parse-bnf-rule (input output closer)
  ;;(format t "I: ~a~%" input)
  ;;(format t "O: ~a~%" output)
  ;;(format t "C: ~a~%~%" closer)
  (let ((part (first input)))
    (cond
      ;; Or Statement : Turn the output into an or-token if not already
      ;; Note that all options of OR statements become implicit groups even if not explicit in the bnf
      ((string= part "|")
       (if (is-or (first output))
           ;; If we're already in an or statement, add a new implicit group
           (progn
             ;; First reverse the last group
             (setf (bnf-token-data (first (bnf-token-data (first output))))
                   (reverse (bnf-token-data (first (bnf-token-data (first output))))))
             ;; Then add the new blank group
             (setf (bnf-token-data (first output)) (cons (make-bnf-token :type 'token-group
                                                                         :data nil)
                                                         (bnf-token-data (first output))))
             ;; Then continue parsing
             (parse-bnf-continue input output closer))
           ;; Otherwise, turn the existing output rule list into an implicit group in the or statement
           (parse-bnf-continue input
                               (list (make-bnf-token :type 'token-or
                                                     :data (list (make-bnf-token :type 'token-group
                                                                                 :data nil)
                                                                 (make-bnf-token :type 'token-group
                                                                                 :data (reverse output)))))
                               closer)))
      ;; Keyword
      ((and (null closer) ;; If we're toplevel
            (eq (length input) 1) ;; And there's only one word in the input
            (null output) ;; But not because we've already processed the others
            (not (string-equal ";" (strip-string  part))) ;; And we explicity ignore the "null_stmt = ';'" rule
            (or  (uiop/utility:string-enclosed-p #\' part #\') ;; And the content is a quoted literal
                 (uiop/utility:string-enclosed-p #\" part #\")))
       (parse-bnf-continue input
                           (output-add output (make-bnf-token :type 'token-keyword
                                                              :data (intern (strip-string part))))
                           closer))
      ;; Literal string
      ((or  (uiop/utility:string-enclosed-p #\' part #\')
            (uiop/utility:string-enclosed-p #\" part #\"))
       (parse-bnf-continue input
                           (output-add output (make-bnf-token :type 'token-literal
                                                              :data (strip-string part)))
                           closer))
      ;; Group
      ((string= part "(")
       (multiple-value-bind (input-new output-new)
           (parse-bnf-rule (rest input) (list) ")")
         (parse-bnf-continue input-new
                             (output-add output (make-bnf-token :type 'token-group
                                                                :data output-new))
                             closer)))
      ;; Optional
      ((string= part "[")
       (multiple-value-bind (input-new output-new )
           (parse-bnf-rule (rest input) (list) "]")
         (parse-bnf-continue input-new
                             (output-add output (make-bnf-token :type 'token-optional
                                                   :data output-new))
                             closer)))
      ;; Repeating
      ((string= part "{")
       (multiple-value-bind (input-new output-new)
           (parse-bnf-rule (rest input) (list) "}")
         (parse-bnf-continue input-new
                             (output-add output (make-bnf-token :type 'token-repeating
                                                                :data output-new))
                             closer)))
      ;; Default, rule expansion
      (t
       (parse-bnf-continue input
                           (output-add output (make-bnf-token :type 'token-rule
                                                              :data part))
                           closer)))
    ))

(defun get-keywords (dict)
  (let ((output (list)))
    (maphash #'(lambda (key val)
                 (when (and (eq  (length val) 1)
                            (string= key (string-upcase key))
                            (eq (bnf-token-type (first val)) 'token-literal))
                   (setf output (cons (bnf-token-data (first val)) output))))
             dict)
    output))


(defun convert-dict-keywords (dict keywords)
  (mapcar #'(lambda (keyword)
              (setf (gethash (string-upcase keyword) dict) (intern keyword)))
          keywords)
  dict)


(defun parse-bnf (filename)
  "Loads a .bnf grammar file and returns a list of rules"
  (let ((lines (load-file-lines filename))
        (rules (make-hash-table :test #'equal))
        (keywords (list)))
    (loop for line in lines do
          ;; Blunt fix for how CL loads escape characters from the bnf file
          (setf line (cl-ppcre:regex-replace-all "\\\\(.)" line "\\"))
          (let* ((parts (cl-utilities:split-sequence #\Space line))
                 (name (first parts))
                 (content (cddr (butlast parts))))
            (multiple-value-bind (remaining-input output)
                (parse-bnf-rule content (list) nil)
              (declare (ignore remaining-input))
              (setf (gethash name rules) output)
              (when (eq (bnf-token-type (first output)) 'token-keyword)
                ;;(format t "Keyword : ~a~%" name)
                (setf keywords (cons name keywords))))))
    (values rules keywords)))
         
