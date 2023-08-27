(in-package :cl-ifc-gen)

(defun test-type-simple-boolean (type)
  (format t "~%Simple boolean : ~a~%" type)
  (prove:plan 6)
  (prove:ok (typep t type) type)
  (prove:ok (typep nil type) type)
  (prove:ok (not (typep 1 type)) type)
  (prove:ok (not (typep 1.0 type)) type)
  (prove:ok (not (typep 'symbol type)) type)
  (prove:ok (not (typep "string" type)) type)
  (prove:finalize))


(defun test-type-simple-real (type)
  (format t "~%Simple real : ~a~%" type)
  (prove:plan 6)
  (prove:ok (typep 4 type) type)
  (prove:ok (typep 2.0 type) type)
  (prove:ok (not (typep t type)) type)
  (prove:ok (not (typep nil type)) type)
  (prove:ok (not (typep 'symbol type)) type)
  (prove:ok (not (typep "string" type)) type)
  (prove:finalize))


;; Waiting on IfcLabel type
(defun test-type-IfcBoxAlignment ()
  (format t "~%Type IfcBoxAlignment")
  (prove:plan 8)
  (prove:ok (typep "top-left" 'cl-ifc::|IfcBoxAlignment|))
  (prove:ok (typep "center" 'cl-ifc::|IfcBoxAlignment|))
  (prove:ok (typep "bottom-right" 'cl-ifc::|IfcBoxAlignment|))

  (prove:ok (not (typep 2 'cl-ifc::|IfcBoxAlignment|)))
  (prove:ok (not (typep 'center 'cl-ifc::|IfcBoxAlignment|)))
  (prove:ok (not (typep "center2" 'cl-ifc::|IfcBoxAlignment|)))
  (prove:ok (not (typep t 'cl-ifc::|IfcBoxAlignment|)))
  (prove:ok (not (typep nil 'cl-ifc::|IfcBoxAlignment|)))  
  (prove:finalize))


(defun test-type-IfcComplexNumber ()
  (format t "~%Type IfcComplexNumber")
  (prove:plan 12)
  (prove:ok (typep '#(1 2) 'cl-ifc::|IfcComplexNumber|))
  (prove:ok (typep '#(100 200) 'cl-ifc::|IfcComplexNumber|))
  (prove:ok (typep '#(0.6 -3.87) 'cl-ifc::|IfcComplexNumber|))
  (prove:ok (typep '#(4) 'cl-ifc::|IfcComplexNumber|))
  (prove:ok (typep '#(4000) 'cl-ifc::|IfcComplexNumber|))
  (prove:ok (typep '#(4.2) 'cl-ifc::|IfcComplexNumber|))

  (prove:ok (not (typep '#(1 2 3) 'cl-ifc::|IfcComplexNumber|)))
  (prove:ok (not (typep '#() 'cl-ifc::|IfcComplexNumber|)))
  (prove:ok (not (typep '#(#\a #\b) 'cl-ifc::|IfcComplexNumber|)))
  (prove:ok (not (typep '#("Hello" "World") 'cl-ifc::|IfcComplexNumber|)))

  (prove:ok (not (typep 5 'cl-ifc::|IfcComplexNumber|)))
  (prove:ok (not (typep "Hello World" 'cl-ifc::|IfcComplexNumber|)))
  (prove:finalize))


(defun run-type-tests ()
  (format t "Testing Type Definitions~%")
  (test-type-simple-real 'cl-ifc::|IfcAbsorbedDoseMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAccelerationMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAmountOfSubstanceMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAngularVelocityMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAreaMeasure|)
  (test-type-simple-boolean 'cl-ifc::|IfcBoolean|)
  (test-type-IfcBoxAlignment)
  (test-type-IfcComplexNumber))


(defun run-cl-ifc-tests ()
  (setf prove:*default-reporter* :fiveam)
  (run-type-tests))
