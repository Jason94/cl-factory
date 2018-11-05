(in-package :cl-factory-test)

(declaim (optimize (speed 0) (safety 0) (debug 3)))

(defvar +default-foo+ "default-foo")
(defvar +default-bar+ "default-bar")
(defvar +factory-foo+ "factoried-foo")
(defvar +factory-bar+ "factoried-bar")

;;;; Test basic CLOS factory definitions

(def-suite class-factory-suite :description "Test basic functionality of factories for CLOS classes")

(in-suite class-factory-suite)

(test empty-class
  "An empty factory on a class with no slots makes an instance of that class"
  (defclass empty-class ()
    ())
  (define-factory 'empty-class)
  (is (eql
       'empty-class
       (type-of (build 'empty-class)))))

(defclass with-default-slots ()
  ((foo
    :initarg :foo
    :initform +default-foo+
    :reader foo)
   (bar
    :initarg :bar
    :initform +default-bar+
    :reader bar)))

(define-factory 'with-default-slots)

(test default-slots
  "An empty factory on a class with default slots, built with no options, has
   the default slot values"
  (let ((instance (build 'with-default-slots)))
    (is (equal +default-foo+
               (foo instance)))
    (is (equal +default-bar+
               (bar instance)))))

(test slot-args
  "An empty factory on a class with default slots, built with options,
   takes those options as values for slots."
  (let ((instance (build 'with-default-slots :bar +factory-bar+)))
    (is (equal +default-foo+
               (foo instance)))
    (is (equal +factory-bar+
               (bar instance)))))

(cl-factory::clear-factories)

(define-factory 'with-default-slots
  :foo +factory-foo+)

(test non-empty-factory
  "A factory that specifies slots on a class with slots uses those slots
   by default"
  (let ((instance (build 'with-default-slots)))
    (is (equal +factory-foo+
               (foo instance)))
    (is (equal +default-bar+
               (bar instance)))))

(test non-empty-factory-and-unspecified
  "A factory that specifies slots on a class can build values for other
   slots"
  (let ((instance (build 'with-default-slots :bar +factory-bar+)))
    (is (equal +factory-bar+
               (bar instance)))))

(test build-vals-override-defaults
  "Slot values specified in build should override those from the factory"
  (let ((instance (build 'with-default-slots :foo "custom-foo")))
    (is (equal "custom-foo"
               (foo instance)))))

(def-suite class-factory-edge-suite :description "Test edge cases for the factories for CLOS classes")

(in-suite class-factory-edge-suite)

(defvar *count* 0)

(cl-factory::clear-factories)
(define-factory 'with-default-slots
  :foo (+ 1 2))

(test slots-eval-at-build
  "A default slot evaluates at build, not at factory-definition."
  (setf *count* 0)
  (cl-factory::clear-factories)
  (define-factory 'with-default-slots
    :foo *count*)
  (incf *count*)
  (is (equal *count*
             (foo (build 'with-default-slots))))
  (incf *count*)
  (is (equal *count*
             (foo (build 'with-default-slots)))))

(run! 'class-factory-suite)
(run! 'slots-eval-at-build)
