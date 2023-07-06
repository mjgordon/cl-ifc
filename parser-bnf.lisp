(in-package :cl-ifc-gen)


(deftype bt-type ()
  `(member :token-none
           :token-keyword
           :token-literal
           :token-rule
           :token-group
           :token-or
           :token-optional
           :token-repeating))

(deftype bt-data ()
  `(or list 
       bnf-token
       symbol
       string))

(defstruct bnf-token
  (type :token-none :type bt-type)
  (data  nil :type bt-data))

(defun is-or (item)
  (and (bnf-token-p item)
       (eq (bnf-token-type item) :token-or)))


(defun output-add (output item)
  "If the output currently being built is an options token, add to the internal data list. Otherwise, add to the raw output list"
  (progn 
    (if (is-or (first output))
        (setf (bnf-token-data (first (bnf-token-data (first output))))
              (cons item (bnf-token-data (first (bnf-token-data (first output))))))
        (setf output (cons item output)))
    output))


;; parse-bnf-rule and parse-bnf-continue call each other in a circle,
;; so we need to forward-declare parse-bnf-rule
(declaim (ftype (function (list list (or simple-array null)) (values t t)) parse-bnf-rule))
(defun parse-bnf-continue (input output closer)
  "Called after each possible type of bnf token, decides whether to continue"
  (if (or (null (rest input))
          (string= (second input) closer))
      (progn
        (if (is-or (first output))
            (setf (bnf-token-data (first output)) (reverse (bnf-token-data (first output))))
            (setf output (reverse output)))
        
        (values (cdr input) output))
      (parse-bnf-rule (rest input) output closer)))


(defun parse-bnf-rule (input output closer)
  "Parse a single line from the bnf file"
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
             (setf (bnf-token-data (first output)) (cons (make-bnf-token :type :token-group
                                                                         :data nil)
                                                         (bnf-token-data (first output))))
             ;; Then continue parsing
             (parse-bnf-continue input output closer))
           ;; Otherwise, turn the existing output rule list into an implicit group in the or statement
           (parse-bnf-continue input
                               (list (make-bnf-token :type :token-or
                                                     :data (list (make-bnf-token :type :token-group
                                                                                 :data nil)
                                                                 (make-bnf-token :type :token-group
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
                           (output-add output (make-bnf-token :type :token-keyword
                                                              :data (intern (string-upcase (strip-string part)))))
                           closer))
      ;; Literal string
      ((or  (uiop/utility:string-enclosed-p #\' part #\')
            (uiop/utility:string-enclosed-p #\" part #\"))
       (parse-bnf-continue input
                           (output-add output (make-bnf-token :type :token-literal
                                                              :data (strip-string part)))
                           closer))
      ;; Group
      ((string= part "(")
       (multiple-value-bind (input-new output-new)
           (parse-bnf-rule (rest input) (list) ")")
         (parse-bnf-continue input-new
                             (output-add output (make-bnf-token :type :token-group
                                                                :data output-new))
                             closer)))
      ;; Optional
      ((string= part "[")
       (multiple-value-bind (input-new output-new )
           (parse-bnf-rule (rest input) (list) "]")
         (parse-bnf-continue input-new
                             (output-add output (make-bnf-token :type :token-optional
                                                   :data output-new))
                             closer)))
      ;; Repeating
      ((string= part "{")
       (multiple-value-bind (input-new output-new)
           (parse-bnf-rule (rest input) (list) "}")
         (parse-bnf-continue input-new
                             (output-add output (make-bnf-token :type :token-repeating
                                                                :data output-new))
                             closer)))
      ;; Default, rule expansion
      (t
       (parse-bnf-continue input
                           (output-add output (make-bnf-token :type :token-rule
                                                              :data part))
                           closer)))
    ))


(defmacro define-keyword-type (&rest keywords)
  (let ((keyword-symbols (mapcar #'intern keywords))) 
    `(deftype keyword-type ()
       (member @,keyword-symbols)
       )))


(defun parse-bnf (filename)
  "Loads a .bnf grammar file and returns a dictionary of rules and a list of keywords"
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
              (when (eq (bnf-token-type (first output)) :token-keyword)
                ;;(format t "Keyword : ~a~%" name)
                (setf keywords (cons name keywords))))))
    (values rules keywords)))
         
