;;;; toy-parser.asd

(asdf:defsystem #:toy-parser
  :serial t
  :description "A toy-parser which turns into a native compiler"
  :author "Dimitri Fontaine"
  :license "WFTPL"
  :depends-on (#:esrap)
  :components ((:file "package")
               (:file "toy-parser")))

