(in-package :cl-ifc-gen)


(defun non-semantic-rule-p (rule)
  (member rule '(declaration
                 digit
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


(defun compile-rule (rule output)
  (cond
    ((string-equal rule "type_decl")
     (deftype cl-ifc::mytype () '(real))
     (format t "~a~%" (length output))
     (format t "~a~%" (type-of (first output)))
     (format t "~a~%" (first output))
     (format t "~a : ~a~%" rule output))))
