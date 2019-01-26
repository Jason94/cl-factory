(in-package :cl-user)

(defpackage cl-factory-system
  (:use :cl :asdf))
(in-package :cl-factory-system)

(ql:quickload :fiveam)

(defsystem "cl-factory"
  :version "0.1.0"
  :description "Factory library for Common Lisp."
  :author "Jason Walker <JasonW94@gmail.com>"
  :pathname "src/"
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "factories" :depends-on ("package" "util")))
  :in-order-to ((test-op (test-op "cl-factory/tests"))))

(defsystem "cl-factory/tests"
  :depends-on ("fiveam" "cl-factory")
  :pathname "test/"
  :components
  ((:file "package")
   (:file "tests-utils" :depends-on ("package"))
   (:file "tests-clos" :depends-on ("package"))
   (:file "tests-plist" :depends-on ("package"))
   (:file "readme" :depends-on ("package")))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-factory-test :run-factory-tests)))
