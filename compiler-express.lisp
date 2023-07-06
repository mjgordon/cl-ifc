(in-package :cl-ifc-gen)


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
        (typecase data
          (list
           (if (eq (length data) 1)
               (if (eq (exp-token-name (first data)) goal-rule)
                   (first data)
                   (token-drill (first data) goal-rule))
               (loop named layer for child in data
                     when (and (not (typep child 'character)) (token-drill child goal-rule))
                     do (return-from layer child))))
          (character nil)
          (nil nil)))))

(defun data-dive (data count)
  (if (= 0 count)
      data
      (data-dive (first (exp-token-data data)) (- count 1))))


(defun build-deftype (type-id underlying-type where-clause)
;;  (format t "Underlying type : ~a~%" underlying-type)
  `(deftype ,type-id ()
     ,(if-let (concrete-type (token-drill underlying-type 'concrete_types))
        (case (exp-token-name (first (exp-token-data concrete-type)))
          (type_ref
           (let ((ref-name (exp-token-data (data-dive  concrete-type 3))))
             `',(intern (string-from-char-list ref-name) :cl-ifc)))
          (simple_types
           (let* ((simple-type (first (exp-token-data concrete-type)))
                  (simple-type-name (exp-token-name (first (exp-token-data simple-type)))))
             (case simple-type-name
               (boolean_type ''boolean)
               (real_type ''real)))))
         
        nil)))


(defun compile-rule (rule input)
  (cond
    ((eq rule 'type_decl)
     (let ((type-id (intern (string-from-char-list (exp-token-data (first (exp-token-data (second input)))))
                            :cl-ifc))
           (underlying-type (fourth input))
           (where-clause (sixth input)))
       ;;(format t "~a : ~a~%" (type-of type-id) type-id)
       (format t "~a : ~a~%" type-id (build-deftype type-id underlying-type where-clause))
       (eval (build-deftype type-id underlying-type where-clause))))))
