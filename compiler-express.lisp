(in-package :cl-ifc-gen)

;;; === Helper functions ===

(defun non-semantic-rule-p (rule)
  (member rule '(declaration
                 digit
                 digits
                 letter
                 not_paren_star_quote_special
                 not_quote
                 schema_body)))


(defun compileable-rule (rule)
  (not (null 
        (member rule '(entity_decl
                       function_decl
                       rule_decl
                       type_decl
                       schema_id)))))

(defun token-layer (token depth)
  (if (<= depth 0)
      token
      (token-layer  (first (exp-token-data token)) (- depth 1))))


(defmacro explore-token (token-original depth &rest body)
  "Syntax for working with a token tree of unknown structure
Each 'is' expression calls its body if the name of the current token level matches its first argument.
The body can use '?level-name' and '?level-data' to refer to the contents of the current level's token"
  `(let* ((?level-token (token-layer ,token-original (- ,depth 1)))
          (?level-name (exp-token-name ?level-token))
          (?level-data (exp-token-data ?level-token)))
     (declare (ignorable ?level-name ?level-data))
     ;;(format t "Depth : ~a Name : ~a~%" ,depth ?level-name)
     (cond ,@(loop :for clause :in body :collect
                   (let ((verb (first clause))
                         (check (second clause))
                         (clause-body (cddr clause)))
                     (if (eq verb 'is)
                         `((eq (exp-token-name (token-layer ,token-original ,depth)) ,check)
                            (explore-token ,token-original ,(+ 1 depth) ,@clause-body))
                         `(t ,clause)))))))


(defun compile-numeric-expression (token-numeric-expression)
  "Solves for a numeric expression at compile-time to include a constant in the output"
;;  (format t "Numeric Expression : ~a~%" token-numeric-expression)
  (explore-token token-numeric-expression 0
                 (is 'numeric_expression
                     (is 'simple_expression
                         (is 'term
                             (is 'factor
                                 (is 'simple_factor
                                     (is 'primary
                                         (is 'literal
                                             (is 'real_literal
                                                 (is 'integer_literal
                                                     (parse-integer (format nil "~a" (first ?level-data))))))))))))))



(defun token-drill (token goal-rule)
  "Returns true if an exp-token ultimately arrives at 'goal-rule'"
  (if (eq  (exp-token-name token) goal-rule)
      token
      (let ((data (exp-token-data token)))
        (if (eq (length data) 1)
            (cond ((characterp (first data))
                   nil)
                  ((eq (exp-token-name (first data)) goal-rule)
                   (first data))
                  (t (token-drill (first data) goal-rule)))
            (loop named layer for child in data
                  when (and (not (typep child 'character)) (token-drill child goal-rule))
                  do (return-from layer child))))))


(defun data-dive (data count)
  "Returns an exp-token 'count' number of data layers deep"
  (if (= 0 count)
      data
      (data-dive (first (exp-token-data data)) (- count 1))))


;;; === Compilation functions ===


(defun build-aggregate (aggregate)
  (let ((entries (list)))
    (loop :for entry :in (strip-sequence  (exp-token-data aggregate)) :by #'cddr :do
          (if-let (literal (token-drill entry 'simple_string_literal)) ;; ?
            (setf entries (cons (strip-sequence (string-from-char-list (exp-token-data literal))) entries))))
    entries))


(defun build-type-predicate (wr type-id)
  (let* ((rule-label (string-from-char-list (exp-token-data (first (exp-token-data (first (exp-token-data wr)))))))
         (predicate-name (intern  (format nil "predicate-~a-~a" type-id rule-label) :cl-ifc))
         ;; First is rule label
         ;; Second is colon
         (rule-expression (third (exp-token-data wr))))
    (when (> (length (exp-token-data rule-expression)) 1)
      (let (;; (expression-start (first (exp-token-data rule-expression))) For now assumed to be SELF
            (rel-op-expression (second (exp-token-data rule-expression)))
            (expression-end (third (exp-token-data rule-expression))))
        (when (token-drill rel-op-expression 'in)
          (values predicate-name
                  `(defun ,predicate-name (object)
                     (and
                      (stringp object) ;; TODO: this and the test need to follow the expected underlying type
                      (member object (list ,@(build-aggregate (token-drill expression-end 'aggregate_initializer)))
                              :test #'string=)))))))))


(defun build-array-type (type-id array-type)
  (let* (;; First is 'ARRAY'
         (bound-spec (second array-type))
         (bound-1 (compile-numeric-expression (token-layer (second (exp-token-data bound-spec)) 1)))
         (bound-2 (compile-numeric-expression (token-layer (fourth (exp-token-data bound-spec)) 1)))
         ;; Third is 'OF'
         (instantiable (cdddr array-type))
         (flag-optional nil)
         (flag-unique nil)
         (predicate-name (intern (format nil "~a-array-p" type-id) :cl-ifc)))
    
    (when (eq (exp-token-name (first instantiable)) 'OPTIONAL)
      (setf instantiable (rest instantiable))
      (setf flag-optional t))

    (when (eq (exp-token-name (first instantiable)) 'UNIQUE)
      (setf instantiable (rest instantiable))
      (setf flag-unique t))

    ;;(format t "Instantiable : ~a~%" instantiable)

    (values predicate-name
            `(defun ,predicate-name (object)
               (and (arrayp object)
                    (flet ((tempp (obj)
                             (typep obj ',(build-underlying-type (first instantiable)))))
                      (every #'tempp object))
                    (>= (length object) ,bound-1)
                    (<= (length object) ,bound-2)))
            )
    ))


(defun build-underlying-type (underlying-type)
  ;;(format t "Underlying type : ~a~%" underlying-type)
  (explore-token underlying-type 0
                 (is 'instantiable_type
                     (build-underlying-type (first ?level-data)))
                 (is 'concrete_types
                     (is 'aggregation_types
                         (is 'array_type '(array)))
                     (is 'simple_types
                         (is 'boolean_type 'boolean)
                         (is 'real_type 'real)
                         (is 'string_type 'string))
                     (is 'type_ref
                         (intern (string-from-char-list (exp-token-data (first (exp-token-data (first ?level-data))))) :cl-ifc)))))


;; Predicate is necessary if there is a where clause, or if underlying is an aggregation type
;; Each can exist in isolation, so:
;; - Collect aggregation predicates
;; - combine with where predicates
;; - If at least one, create the expanded type
;; - Otherwise create the simple type
(defun build-deftype (type-id underlying-type where-clause)
  "Returns a deftype code string, to be used with (eval)"
  (let ((predicate-names (list))
        (predicate-defuns (list)))
    ;; First check for arrays, etc
    (explore-token underlying-type 0
                   (is 'concrete_types                   
                       (is 'aggregation_types
                           (is 'array_type
                               (multiple-value-bind (p-name p-defun)
                                   (build-array-type type-id ?level-data)
                                 (setf predicate-names (cons p-name predicate-names))
                                 (setf predicate-defuns (cons p-defun predicate-defuns)))))))
    ;; Then check for where clauses
    (when (token-drill where-clause 'where)
      (loop :for rule :in (rest (exp-token-data where-clause)) :by #'cddr :do
            (multiple-value-bind (p-name p-defun)
                (build-type-predicate rule type-id)
              (setf predicate-names (cons p-name predicate-names))
              (setf predicate-defuns (cons p-defun predicate-defuns)))))
    
    (let* ((underlying-primary (build-underlying-type underlying-type))
           (and-clause `(and ,underlying-primary
                             ,@(mapcar (lambda (name)
                                         `(satisfies ,name))
                                       predicate-names)))) 
      `(progn ,@predicate-defuns
              (deftype ,type-id ()
                ',and-clause)))))


(defun compile-type-decl (input)
  (declare (optimize (debug 3)))
  (let (;; First is 'TYPE' keyword
        (type-id (intern (string-from-char-list (exp-token-data (first (exp-token-data (second input)))))
                         :cl-ifc))
        ;; Third is equals sign
        (underlying-type (first (exp-token-data  (fourth input))))
        ;; Fifth is semicolon
        (where-clause (sixth input)))

    ;; Test specific 
    (when (eq type-id 'cl-ifc::|IfcAbsorbedDoseMeasure2|)
      ;;(defparameter *test-type* input)
      ;;(defparameter *test-underlying* (fourth *complex-test*))
      (format t "~%~a~%" input))

    (let ((deftype-code (build-deftype type-id underlying-type where-clause)))
      ;;(format t "Underlying name : ~a ~%" (exp-token-name underlying-type))
      (format t "~a : ~a~% ~%" type-id deftype-code)
      (eval deftype-code))))

(defun compile-bnf-rule (rule input)
  (cond
    ((eq rule 'type_decl)
     (compile-type-decl input))))
