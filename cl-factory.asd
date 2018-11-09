(in-package :cl-user)
(defpackage cl-factory-system
  (:use :cl :asdf))
(in-package :cl-factory-system)

(ql:quickload :fiveam)

(asdf:defsystem "cl-factory"
  :version "0.01"
  :description ""
  :author "Jason Walker <JasonW94@gmail.com>"
  :pathname "src/"
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "factories" :depends-on ("package" "util"))))

(defsystem "cl-factory/test"
  :depends-on ("fiveam" "cl-factory")
  :pathname "test/"
  :components
  ((:file "test-suite")
   (:file "tests" :depends-on ("test-suite"))
   (:file "tests-plist" :depends-on ("test-suite"))))
