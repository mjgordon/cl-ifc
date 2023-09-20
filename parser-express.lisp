(in-package :cl-ifc-gen)

(defparameter *bnf-dict* nil)
(defparameter *bnf-keywords* nil)
(defparameter *exp-characters* nil)


;;; === Helper Functions ===

(defstruct exp-token
  (name nil :type symbol)
  data)


(defmacro prepend (list item &optional (indent 0))
  `(progn
     (print-debug (format nil  "PREPEND ~a" ,item) 2  ,indent)
     (setf ,list (cons ,item ,list))))


(defun compileable-rule (rule)
  (not (null 
        (member rule '(entity_decl
                       function_decl
                       rule_decl
                       type_decl
                       schema_id)))))


(defun non-semantic-rule-p (rule)
  "Non-semantic rules don't produce new tokens"
  (member rule '(declaration
                 digit
                 letter
                 not_paren_star_quote_special
                 not_quote
                 schema_body)))


(defun compare-literal (literal input output &optional (input-raw input) (indent 0))
  "Checks if 'literal' is present at the beginning of the list 'input'. 
Returns an mv of equality and the remaining input if true, or the original input if false"
  (if (if (or (typep (first literal) 'symbol) (typep (first input) 'symbol))
          (eq (first literal) (first input))
          (char-equal (first literal) (first input)))
      (progn 
        (prepend output (first input) indent)
        (if (rest literal)
            (compare-literal (rest literal) (rest input) output input-raw indent)
            (values t (rest input) output)))
      (values nil input-raw output)))


;;; === Parse Functions ===


;; Token types are OR, LITERAL, GROUP, OPTIONAL, REPEATING, RULE
(defun parse-main (grammar input output rule-depth)
  (let ((grammar-head (first grammar)))
    ;;    (format t "Grammar : ~a~%" grammar)
    (cond
      ;; EXP file is currently starting a comment
      ;; TODO convert this to a check for the 'remark' rule
      ;; First try just putting it first created an infinite loop 
      ((compare-literal (char-list-literal "(*") input output input rule-depth)
       (print-debug "SKIP comment" 3 rule-depth)
       (parse-continue (cons 'comment-dummy grammar) (consume-comment input output rule-depth) output rule-depth))
      ;; Token type GROUP
      ;; Turned off debug print scoping, may not be necessary/meaningful for this one
      ((eq (bnf-token-type grammar-head) :token-group)
       ;;(print-debug "GROUP 4 {" rule-depth)
       (multiple-value-if (parse-main (bnf-token-data grammar-head) input output (+ 1 rule-depth))
                          (input-new output-new)
                          (progn
                            ;;(print-debug "} GROUP Success" 4 rule-depth)
                            (parse-continue grammar input-new output-new rule-depth))
                          (progn
                            ;;(print-debug "} GROUP Failure" 4 rule-depth)
                            (values nil input output))))

      ;; Token type KEYWORD
      ((eq (bnf-token-type grammar-head) :token-keyword)
       (if (eq (bnf-token-data grammar-head) (first input))
           (progn
             (print-debug "KEYWORD Success" 1 rule-depth)
             (parse-continue grammar (rest input) output rule-depth))
           (progn
             (print-debug "KEYWORD Failure" 1 rule-depth)
             (values nil input output))))
      
      ;; Token type LITERAL
      ;; Doesn't use any debug printing as the checking through every letter for simple_ids would make it unreadable
      ((eq (bnf-token-type grammar-head) :token-literal)
       ;;(print-debug (format nil "Grammar Construct : Literal ~a" (bnf-token-data grammar-head)))
       (multiple-value-if (compare-literal (char-list (bnf-token-data grammar-head)) input output input rule-depth)
                          (input-new output-new)
                          (parse-continue grammar input-new output-new rule-depth)
                          (values nil input output))) ;; Exit : Literal matching failed
      
      
      ;; Token type OPTIONAL
      ((eq (bnf-token-type grammar-head) :token-optional)
       (print-debug "OPTIONAL {" 3 rule-depth)
       (multiple-value-if (parse-optional (bnf-token-data grammar-head) input output rule-depth)
                          (input-new output-new)
                          (progn
                            (print-debug "} OPTIONAL Success" 3 rule-depth)
                            (parse-continue grammar input-new output-new rule-depth))
                          (progn
                            (print-debug "} OPTIONAL Failure" 3 rule-depth)
                            (parse-continue grammar input output rule-depth))))
      ;; Token type OR
      ((eq (bnf-token-type grammar-head) :token-or)
       (print-debug "OR " 3 rule-depth)
       (multiple-value-if (parse-or (bnf-token-data grammar-head) input output rule-depth)
                          (input-new output-new)
                          (parse-continue grammar input-new output-new rule-depth)
                          (values nil input output)))
      ;; Token type REPEATING
      ((eq (bnf-token-type grammar-head) :token-repeating)
       (print-debug "REPEATING {" 3 rule-depth)
       (multiple-value-if (parse-repeating (bnf-token-data grammar-head) input output rule-depth)
                          (input-new output-new)
                          (progn
                            (print-debug "} REPEATING Success" 3 rule-depth)
                            (parse-continue grammar input-new output-new rule-depth))
                          (progn
                            (print-debug "} REPEATING Failure" 3 rule-depth)
                            (values nil input output))))

      ;; Token type RULE
      ((eq (bnf-token-type grammar-head) :token-rule)
       (let* ((rule-name (bnf-token-data grammar-head))
              (rule-symbol (intern (string-upcase rule-name))))
         (print-debug (format nil "Rule : ~a {" rule-name) 3 rule-depth)
         (multiple-value-if (parse-main (gethash rule-name *bnf-dict*) input (list) (+ 1 rule-depth))
                            (input-new output-new)
                            (progn
                              (print-debug (format nil "} Success ~a" rule-name) 3 rule-depth)
                              (if (compileable-rule rule-symbol)
                                  (progn
                                    (compile-bnf-rule rule-symbol (reverse output-new))
                                    (parse-continue grammar input-new (list) rule-depth))
                                  (progn
                                    (if (non-semantic-rule-p rule-symbol)
                                        (prepend output (first output-new))
                                        (prepend output (make-exp-token :name rule-symbol
                                                                        :data (reverse output-new))))
                                    (parse-continue grammar input-new output rule-depth))))
                            (progn
                              (print-debug (format nil "} Failed ~a" rule-name) 3 rule-depth)
                              (values nil input output)))))
      )))


(defun parse-continue (grammar input output rule-depth)
  "Called in the success condition of various token types within parse-main. 
Continues or exits depending on whether there is still grammar to check"
  (if (null input)
      (values t input output) 
      ;; If whitespace, drop first character and recurse
      (if (member (first input) (list #\Space #\Return #\Tab))
          (progn
            (print-debug (format nil "SKIP whitespace : ~a" (first input)) 3 rule-depth)
            (parse-continue grammar (rest input) output rule-depth))
          ;; If theres more to parse, call main again, otherwise return
          (if (rest grammar)
              (parse-main (rest grammar) input output rule-depth)
              (values t input output)))))


(defun parse-optional (grammar-item input output rule-depth)
  (multiple-value-if (parse-main grammar-item input output (+ 1 rule-depth))
         (input-new output-new)
         (values t input-new output-new)
         (values nil input output)))


(defun parse-or (grammar-options input output rule-depth)
  (multiple-value-if (parse-main (list (first grammar-options)) input output (+ 1 rule-depth))
         (input-new output-new)
         (values t input-new output-new)
         (if (rest grammar-options)
             (parse-or (rest grammar-options) input output rule-depth)
             (values nil input output))))


(defun parse-repeating (grammar-item input output rule-depth)
  (multiple-value-if (parse-main grammar-item input output (+ 1 rule-depth))
                     (input-new output-new)
                     (progn
                       (print-debug "REPEATING CONTINUE" 3 rule-depth)
                       (parse-repeating grammar-item input-new output-new rule-depth))
                     (values t input output)))


(defun consume-comment (input output rule-depth)
  "Assuming input is at the start of a comment, return input from the end of the comment"
  (multiple-value-if (compare-literal (char-list-literal "*)") input output input (+ 1 rule-depth))
         (input-new output-new)
         (values input-new output-new)
         (consume-comment (rest input) output rule-depth)))


(defun kic-p (character)
  "Predicate for Keyword Indicator Character. Discounts a possible keyword if appears on either side"
  (or (alphanumericp character)
      (char= character #\_)
      (char= character #\')))


(defun compare-keyword (literal input &optional (input-raw input))
  (if (char-equal (first literal) (first input))
      (if (rest literal)
          (compare-keyword (rest literal) (rest input) input-raw)
          (if (kic-p (second input))
              (values nil input-raw)
              (values t (rest input))))
      (values nil input-raw)))


(defun convert-exp-keywords (input input-last output keywords &optional (remaining-keywords keywords))
  (if (null input)
      (reverse output)
      (multiple-value-bind (compare-success input-new)
          (compare-keyword (char-list (first remaining-keywords)) input)
        (if (and compare-success
                 (not (kic-p input-last)))
            (progn
              ;;(format t "Convert : ~a -> ~a ~%" (first remaining-keywords) (subseq input 0 10))
              (convert-exp-keywords input-new
                                    nil
                                    (cons (intern (string-upcase  (first remaining-keywords))) output) 
                                    keywords))
            
            ;; If not matched
            ;; if there's more keywords to check, check the next
            ;; Otherwise record the character and continue to the next
            (if (rest remaining-keywords)
                (convert-exp-keywords input
                                      input-last
                                      output
                                      keywords
                                      (rest remaining-keywords))
                (convert-exp-keywords (rest input)
                                      (first input)
                                      (cons (first input) output)
                                      keywords))))))

(defun just-load-bnf ()
  (multiple-value-bind (bnf-dict keywords)
      (parse-bnf (asdf:system-relative-pathname :cl-ifc "schemas/express.bnf"))
    (setf *bnf-dict* bnf-dict)
    (setf *bnf-keywords* keywords)
    t))


(defun just-load-express ()
  (let* ((lines (load-file-lines (asdf:system-relative-pathname :cl-ifc "schemas/IFC2X3_TC1.exp")))
         (characters (lines-to-characters lines)))

    ;; Convert all instances of bnf keywords in the character list to symbols
    (format t "Converting keywords~%")
    (set-temp *debug-level* 0 
      (setf characters (convert-exp-keywords characters nil (list) *bnf-keywords*)))

    (setf *exp-characters* characters))
  t)


(defun just-parse-express ()
  (multiple-value-bind (success-p input output)
      (parse-main (gethash "syntax" *bnf-dict*) *exp-characters* (list) 0)
    (format t "~%Parse success : ~a~%" success-p)
    (format t "Parse remaining input length : ~a~%" (length input))
    (format t "Parse output length : ~a~%~%" (length  output))
    (format t "~a~%" output))
  )


(defun parse-express (filename)
  ;; Clear the destination package (the below didn't work right)
  ;;(delete-package :cl-ifc)
  ;;(defpackage :cl-ifc)                                       
  
  ;; First load the grammar dictionary from bnf
  (multiple-value-bind (bnf-dict keywords)
      (parse-bnf (asdf:system-relative-pathname :cl-ifc "schemas/express.bnf"))
    (setf *bnf-dict* bnf-dict) ;; Set to a global variable for debug checking

    ;; Load the .exp file as a flat list of characters
    (let* ((lines (load-file-lines filename))
           (characters (lines-to-characters lines)))

      ;; Convert all instances of bnf keywords in the character list to symbols
      (format t "Converting keywords~%")
      (set-temp *debug-level* 0 
        (setf characters (convert-exp-keywords characters nil (list) keywords)))
      
      (format t "Converting keywords finished~%")
      
      (format t "BNF Rule Count : ~a~%" (hash-table-size *bnf-dict*))
      (format t "Starting input length : ~a~%"  (length characters))

      (multiple-value-bind (success-p input output)
          (parse-main (gethash "syntax" *bnf-dict*) characters (list) 0)
        (format t "~%Parse success : ~a~%" success-p)
        (format t "Parse remaining input length : ~a~%" (length input))
        (format t "Parse output length : ~a~%~%" (length  output))
        (format t "~a~%" output))
      nil )))
