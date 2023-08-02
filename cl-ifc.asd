(asdf:defsystem :cl-ifc
  :name "cl-ifc"
  :depends-on (:cl-utilities :cl-ppcre :prove)
  :components ((:file "packages")
               (:file "utility")
               (:file "parser-bnf")
               (:file "compiler-express")
               (:file "parser-express")
               (:file "tests")))
