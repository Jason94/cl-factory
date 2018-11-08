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
  (is (eql
       '()
       (build 'empty-plist))))

(test empty-factory-build-args
  "An empty factory, built with options, includes those options"
  (cl-factory::clear-factories)
  (define-factory 'empty-plist)
  (is (eql
       +factory-foo+
       (getf (build 'empty-plist :foo +factory-foo+) :foo))))

(test with-default-slots
  "A factory on a plist with default props, built with no options, has those props"
  (cl-factory::clear-factories)
  (define-factory 'basic-plist
    :foo +default-foo+)
  (is (eql
       +default-foo+
       (getf (build 'basic-plist) :foo))))



(run! 'plist-factory-suite)
