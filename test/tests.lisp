(in-package :cl-factory-test)

;;; Test basic CLOS factory definitions

(defclass empty-class ()
  ())

(eql (type-of (type-of (make-instance 'empty-class)))

     'EMPTY-CLASS)

(def-suite class-factory-suite :description "Test the factories for CLOS classes")

(in-suite class-factory-suite)

(test empty-class
  "A factory on a class with no slots makes an instance of that class"
  (define-factory 'empty-class
      )
  (is (eql (type-of (build 'empty-class))
           'empty-class)))

(run! 'class-factory-suite)
