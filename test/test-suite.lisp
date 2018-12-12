(in-package :cl-user)

(defpackage :cl-factory-test
  (:use :cl :cl-factory :it.bese.FiveAM))
(in-package :cl-factory-test)

(def-suite all-factory-tests
    :description "Parent suite for all cl-factory tests.")
