(asdf:defsystem :cl-ifc
  :name "cl-ifc"
  :depends-on (:cl-utilities)
  :components ((:file "packages")
               (:file "utility")
               (:file "parser-bnf")
               (:file "parser-express")))
