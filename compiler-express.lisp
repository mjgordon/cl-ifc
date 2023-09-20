(in-package :cl-ifc-gen)

(defparameter *print-type-defs* t)

(defparameter *verbose-flag* nil)

;;; === Helper Functions ===


(defun token-drill (token path)
  "Returns the first token in the tokens data list 'depth' number of times"
  (if path
      (token-drill (nth (first path) (exp-token-data token)) (rest path))
      token))


(defun token-find (token goal-rule)
  "Returns true if an exp-token ultimately arrives at 'goal-rule'"
  (if (eq  (exp-token-name token) goal-rule)
      token
      (let ((data (exp-token-data token)))
        (if (eq (length data) 1)
            (cond ((characterp (first data))
                   nil)
                  ((eq (exp-token-name (first data)) goal-rule)
                   (first data))
                  (t (token-find (first data) goal-rule)))
            (loop named layer for child in data
                  when (and (not (typep child 'character)) (token-find child goal-rule))
                  do (return-from layer child))))))

;;; === Compilation functions ===


(defmacro explore-token (token-original path &rest body)
  "Syntax for working with a token tree of unknown structure
Each 'is' expression calls its body if the name of the current token level matches its first argument.
The body can use '?level-name' and '?level-data' to refer to the contents of the current level's token"
  `(let* ((?level-token (token-drill ,token-original ,path))
          (?level-name (exp-token-name ?level-token))
          (?level-data (exp-token-data ?level-token)))
     (declare (ignorable ?level-name ?level-data))
     ;;(format t "Depth : ~a Name : ~a~%" ,depth ?level-name)
     (cond ,@(loop :for clause :in body :collect
                   (let ((verb (first clause))
                         (check (second clause))
                         (clause-body (cddr clause)))
                     (case verb
                       (is
                        `((eq (exp-token-name (first ?level-data)) ,check)
                          (explore-token ,token-original (list ,@(append (rest path) (list 0))) ,@clause-body)))
                       (?optional
                        (let ((level-body (first clause-body))
                              (?body (second clause-body)))
                          (declare (ignorable ?body))
                          level-body))
                       (t `,clause)))))))



(defun compile-expression (token-expression)
  (explore-token token-expression nil
                 (is 'simple_expression (compile-simple-expression ?level-token))))


(defun compile-interval (token-interval)
  (let* (;; 0th is open curly brace
         (interval-low (compile-simple-expression (token-drill token-interval '(1 0))))
         (interval-op  (token-drill token-interval '(2)))
         (interval-item (compile-simple-expression (token-drill token-interval '(3 0))))
         (interval-op2  (token-drill token-interval '(4)))
         (interval-high (compile-simple-expression (token-drill token-interval '(5 0)))))

    (when *verbose-flag* (format t "Low : ~a~% Op : ~a~% Item: ~a~% Op2: ~a~%High: ~a~%"
                                 interval-low
                                 interval-op
                                 interval-item
                                 interval-op2
                                 interval-high))))


(defun compile-numeric-expression (token-numeric-expression)
  "Solves for a numeric expression at compile-time to include a constant in the output"
;;  (format t "Numeric Expression : ~a~%" token-numeric-expression)
  (explore-token token-numeric-expression nil
                 (is 'simple_expression (compile-simple-expression ?level-token))))


(defun compile-simple-expression (token-simple-expression)
  (explore-token token-simple-expression nil
                 (is 'term
                     (is 'factor
                         (is 'simple_factor
                             (is 'interval (compile-interval ?level-token)
                                 (?optional 'unary_op
                                            (compile-unary-op ?body)
                                            (is 'primary
                                                (is 'literal
                                                    (is 'real_literal
                                                        (is 'integer_literal
                                                            (is 'digits
                                                                (parse-integer (format nil "~{~a~}" ?level-data))))))))))))))

(defun compile-unary-op (token-unary-op body)
  (if (string= (first (exp-token-data token-unary-op)) "-")
      `(- ,body)
      (format t "Unary op not implemented : ~a~%" token-unary-op)))


(defun get-aggregate-entries (aggregate)
  (let ((entries (list)))
    (loop :for entry :in (strip-sequence  (exp-token-data aggregate)) :by #'cddr :do
          (if-let (literal (token-find entry 'simple_string_literal)) ;; ?
            (setf entries (cons (strip-sequence (string-from-char-list (exp-token-data literal))) entries))))
    entries))


(defun build-predicate-domain-rule (dr type-id)
  (when *verbose-flag* (format t "Domain rule for ~a : ~% ~a~%" type-id dr))
  (let* ((rule-label (string-from-char-list (exp-token-data (first (exp-token-data (first (exp-token-data dr)))))))
         (predicate-name (intern  (format nil "predicate-~a-~a" type-id rule-label) :cl-ifc))
         ;; First is rule label
         ;; Second is colon
         (rule-expression (token-drill dr '(2))))

    (if (> (length (exp-token-data rule-expression)) 1)
        (let (;; (expression-start (first (exp-token-data rule-expression))) For now assumed to be SELF
              (rel-op-expression (second (exp-token-data rule-expression)))
              (expression-end (third (exp-token-data rule-expression))))
          (when (token-find rel-op-expression 'in)
            (values predicate-name
                    `(defun ,predicate-name (object)
                       (and
                        (stringp object) ;; TODO: this and the test need to follow the expected underlying type
                        (member object (list ,@(get-aggregate-entries (token-find expression-end 'aggregate_initializer)))
                                :test #'string=))))))
        (compile-expression rule-expression))))


(defun build-predicate-underlying-array (type-id array-type)
  (let* (;; First is 'ARRAY'
         (bound-spec (second array-type))
         (bound-1 (compile-numeric-expression (token-drill (second (exp-token-data bound-spec)) '(0))))
         (bound-2 (compile-numeric-expression (token-drill (fourth (exp-token-data bound-spec)) '(0))))
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

    (values predicate-name
            `(defun ,predicate-name (object)
               (and (arrayp object)
                    (flet ((tempp (obj)
                             (typep obj ',(build-underlying-type (first instantiable)))))
                      (every #'tempp object))
                    (>= (length object) ,bound-1)
                    (<= (length object) ,bound-2))))))


(defun build-predicate-underlying-list (type-id list-type)
  (let* (;; First is 'LIST'
         (bound-spec (second list-type))
         (bound-1 (compile-numeric-expression (token-drill (second (exp-token-data bound-spec)) '(0))))
         (bound-2 (compile-numeric-expression (token-drill (fourth (exp-token-data bound-spec)) '(0))))
         ;; Third is 'OF'
         (instantiable (cdddr list-type))
         (flag-unique nil)
         (predicate-name (intern (format nil "~a-list-p" type-id) :cl-ifc)))

    (when (eq (exp-token-name (first instantiable)) 'UNIQUE)
      (setf instantiable (rest instantiable))
      (setf flag-unique t))

    (values predicate-name
            `(defun ,predicate-name (object)
               (and (listp object)
                    (flet ((tempp (obj)
                             (typep obj ',(build-underlying-type (first instantiable)))))
                      (every #'tempp object))
                    (>= (length object) ,bound-1)
                    (<= (length object) ,bound-2))))))


(defun build-underlying-type (underlying-type-data)
  ;;(format t "Underlying type : ~a~%" underlying-type-data)
  (explore-token underlying-type-data nil
                 (is 'instantiable_type
                     (build-underlying-type (first ?level-data)))
                 (is 'concrete_types
                     (is 'aggregation_types
                         (is 'array_type '(array))
                         (is 'list_type) 'list)
                     
                     (is 'simple_types
                         (is 'integer_type 'integer)
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
    (explore-token underlying-type nil
                   (is 'concrete_types                   
                       (is 'aggregation_types
                           (is 'array_type
                               (multiple-value-bind (p-name p-defun)
                                   (build-predicate-underlying-array type-id ?level-data)
                                 (setf predicate-names (cons p-name predicate-names))
                                 (setf predicate-defuns (cons p-defun predicate-defuns))))
                           (is 'list_type
                               (multiple-value-bind (p-name p-defun)
                                   (build-predicate-underlying-list type-id ?level-data)
                                 (setf predicate-names (cons p-name predicate-names))
                                 (setf predicate-defuns (cons p-defun predicate-defuns)))))))
    ;; Then check for where clauses
    (when (token-find where-clause 'where)
      (loop :for rule :in (rest (exp-token-data where-clause)) :by #'cddr :do
            (multiple-value-bind (p-name p-defun)
                (build-predicate-domain-rule rule type-id)
              (setf predicate-names (cons p-name predicate-names))
              (setf predicate-defuns (cons p-defun predicate-defuns)))))
    
    (let* ((underlying-primary (build-underlying-type underlying-type))
           (and-clause `(and ,underlying-primary
                             ,@(mapcar (lambda (name)
                                         `(satisfies ,name))
                                       predicate-names))))
      ;; TODO : Replace progn and global predicates with flet
      `(progn ,@predicate-defuns
              (deftype ,type-id ()
                ',and-clause)))))


(defun compile-type-decl (input)
  (declare (optimize (debug 3)))
  (let (;; First is 'TYPE' keyword
        (type-id (intern (string-from-char-list (exp-token-data (first (exp-token-data (second input)))))
                         :cl-ifc))
        ;; Third is equals sign
        (underlying-type (fourth input))
        ;; Fifth is semicolon
        (where-clause (sixth input)))
    
    ;; For dev: get info on a specific type
    (setf *verbose-flag* (eq type-id 'cl-ifc::|IfcCompoundPlaneAngleMeasure|))


    ;;(when *verbose-flag* (format t "~a~%" input))

    (let ((deftype-code (build-deftype type-id underlying-type where-clause)))
      (when *print-type-defs*
        (format t "~a : ~a~% ~%" type-id deftype-code))
      (eval deftype-code))))


(defun compile-bnf-rule (rule input)
  "Compiles an exp-token of a compileable type"
  (cond
    ((eq rule 'type_decl)
     (compile-type-decl input))))
