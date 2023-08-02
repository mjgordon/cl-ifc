(in-package :cl-ifc-gen)

(defun string-member (string)
  (and (typep string 'string)
       (member string '("a" "b" "c") :test 'string=)))

(deftype string-type-test ()
  `(and string
        (satisfies string-member)))


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

(defun build-aggregate (aggregate)
  (let ((entries (list)))
    (loop :for entry :in (exp-token-data aggregate) :do
          (if-let (literal (token-drill entry 'simple_string_literal))
            (setf entries (cons (string-from-char-list (exp-token-data literal)) entries))))
    entries))

(defun build-domain-rule (wr)
  (let ((rule-label (first (exp-token-data wr)))
        (rule-expression (third (exp-token-data wr))))
    (when (> (length (exp-token-data rule-expression)) 1)
      (let ((expression-start (first (exp-token-data rule-expression)))
            (rel-op-expression (second (exp-token-data rule-expression)))
            (expression-end (third (exp-token-data rule-expression))))
        (when (token-drill rel-op-expression 'in)
          `(member ,(build-aggregate (token-drill expression-end 'aggregate_initializer))))))))



(defun build-underlying-type (underlying-type)
  (if-let (concrete-type (token-drill underlying-type 'concrete_types))
     (case (exp-token-name (first (exp-token-data concrete-type)))
       (type_ref
        (let ((ref-name (exp-token-data (data-dive  concrete-type 3))))
          `',(intern (string-from-char-list ref-name) :cl-ifc)))
       (simple_types
        (let ((simple-type-name (exp-token-name (data-dive concrete-type 2)) ))
          (case simple-type-name
            (boolean_type ''boolean)
            (real_type ''real)))))
     
     nil))


(defun build-deftype (type-id underlying-type where-clause)
  "Returns a deftype code string, to be used with (eval)"
  ;;  (format t "Underlying type : ~a~%" underlying-type)
  
  `(deftype ,type-id ()
     ,(if (token-drill where-clause 'where)
          
          `(and ,(build-underlying-type underlying-type)
                ,(loop :for rule :in (rest (exp-token-data where-clause)) :by #'cddr
                       :collect (build-domain-rule rule) ))
          (build-underlying-type underlying-type))))


(defun compile-rule (rule input)
  (cond
    ((eq rule 'type_decl)
     (let ((type-id (intern (string-from-char-list (exp-token-data (first (exp-token-data (second input)))))
                            :cl-ifc))
           (underlying-type (fourth input))
           (where-clause (sixth input)))
       (when (eq type-id 'cl-ifc::|IfcBoxAlignment2|)
         (format t "~a~%" input)
         )
       (let ((deftype-code (build-deftype type-id underlying-type where-clause)))
         (format t "~a : ~a~%" type-id deftype-code)
         (eval deftype-code))))))
