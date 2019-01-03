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

;; To test cl-factory, make sure you have fiveam installed.
;; (ql:quickload :fiveam)

(defsystem "cl-factory/tests"
  :depends-on ("fiveam" "cl-factory")
  :pathname "test/"
  :components
  ((:file "test-suite")
   (:file "tests-utils" :depends-on ("test-suite"))
   (:file "tests-clos" :depends-on ("test-suite"))
   (:file "tests-plist" :depends-on ("test-suite"))
   (:file "readme" :depends-on ("test-suite")))
  ;; TODO: I don't think this is the idiomatic way to run tests.
  :perform (test-op (o c) (funcall #'fiveam:run! 'cl-factory-test::all-factory-tests)))
