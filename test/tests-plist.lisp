(in-package :cl-factory-test)

(defvar +default-foo+ "default-foo")
(defvar +default-bar+ "default-bar")
(defvar +factory-foo+ "factoried-foo")
(defvar +factory-bar+ "factoried-bar")

(def-suite plist-factory-suite :description "Test basic functionality of factories for plists")

(in-suite plist-factory-suite)

(test empty-factory
  "A factory on no class without default params makes an empty plist"
  (cl-factory::clear-factories)
  (define-factory 'empty-plist)
  (is (equal '()
             (build 'empty-plist))))

(test empty-factory-build-args
  "An empty factory, built with options, includes those options"
  (cl-factory::clear-factories)
  (define-factory 'empty-plist)
  (is (equal +factory-foo+
             (getf (build 'empty-plist :foo +factory-foo+) :foo))))

(test default-props
  "A factory on a plist with default props, built with no options, has those props"
  (cl-factory::clear-factories)
  (define-factory 'basic-plist
    :foo +default-foo+)
  (is (equal +default-foo+
             (getf (build 'basic-plist) :foo))))

(test override-default-props
  "A factory on a plist with default props, built with options, takes the options over
   the defaults."
  (cl-factory::clear-factories)
  (define-factory 'basic-plist
    :foo +default-foo+)
  (is (equal +factory-foo+
             (getf (build 'basic-plist :foo +factory-foo+) :foo))))

(test extra-props
  "Building a plist factory can use props not defined in the factory"
  (cl-factory::clear-factories)
  (define-factory 'basic-plist
    :foo +default-foo+)
  (let ((instance (build 'basic-plist :bar +factory-bar+)))
    (is (equal +default-foo+
               (getf instance :foo)))
    (is (equal +factory-bar+
               (getf instance :bar)))))

(defvar *count-plist* 0)

(test props-eval-at-build
  "A default prop evaluates at build, not factory-definition"
  (cl-factory::clear-factories)
  (setf *count-plist* 0)
  (define-factory 'dynamic-plist
    :foo *count-plist*)
  (incf *count-plist*)
  (is (equal 1
             (getf (build 'dynamic-plist) :foo)))
  (incf *count-plist*)
  (is (equal 2
             (getf (build 'dynamic-plist) :foo))))

(run! 'plist-factory-suite)
