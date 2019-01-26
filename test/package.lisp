(in-package :cl-user)

(defpackage #:cl-factory-test
  (:use #:cl #:cl-factory #:it.bese.FiveAM)
  (:export #:run!
           #:all-factory-tests
           #:run-factory-tests))

(in-package :cl-factory-test)

(def-suite all-factory-tests
    :description "Parent suite for all cl-factory tests.")

(defun run-factory-tests ()
  (run! 'all-factory-tests))
