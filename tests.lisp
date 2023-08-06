(in-package :cl-ifc-gen)

(defun test-type-simple-boolean (type)
  (format t "Simple boolean : ~a~%" type)
  (prove:plan 6)
  (prove:ok (typep t type) type)
  (prove:ok (typep nil type) type)
  (prove:ok (not (typep 1 type)) type)
  (prove:ok (not (typep 1.0 type)) type)
  (prove:ok (not (typep 'symbol type)) type)
  (prove:ok (not (typep "string" type)) type)
  (prove:finalize))


(defun test-type-simple-real (type)
  (format t "Simple real : ~a~%" type)
  (prove:plan 6)
  (prove:ok (typep 4 type) type)
  (prove:ok (typep 2.0 type) type)
  (prove:ok (not (typep t type)) type)
  (prove:ok (not (typep nil type)) type)
  (prove:ok (not (typep 'symbol type)) type)
  (prove:ok (not (typep "string" type)) type)
  (prove:finalize))


(defun run-type-tests ()
  (format t "Testing Type Definitions~%")
  (test-type-simple-real 'cl-ifc::|IfcAbsorbedDoseMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAccelerationMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAmountOfSubstanceMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAngularVelocityMeasure|)
  (test-type-simple-real 'cl-ifc::|IfcAreaMeasure|)
  (test-type-simple-boolean 'cl-ifc::|IfcBoolean|))


(defun run-cl-ifc-tests ()
  (setf prove:*default-reporter* :fiveam)
  (run-type-tests))
