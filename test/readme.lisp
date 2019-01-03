(in-package :cl-factory-test)

;;;; Used to provide code snippets for README.md. These are not pulled from unit tests
;;;; so the unit tests are free to change.

(defclass student ()
  ((name
    :initarg :name
    :reader name)
   (gpa
    :initarg :gpa
    :reader gpa)
   (grade-level
    :initarg :grade-level
    :initform 9
    :reader grade-level)))

(define-factory 'student
  :name "John Doe"
  :gpa  3.5)

(define-factory ('bad-student :class 'student)
  :name "Failing Frankie"
  :gpa 2.0)

(defun is-passingp (student)
  (>= (gpa student) 2.5))

(defun amount-to-pass (student)
  (if (is-passingp student)
      0
      (- 2.5 (gpa student))))

(defun in-high-schoolp (student)
  (>= (grade-level student) 9))

(def-suite student-tests)
(in-suite student-tests)

(test is-passingp
  (is (is-passingp (build 'student))))

(test already-passing
  (is (= 0
         (amount-to-pass (build 'student)))))

(test in-high-schoolp
  (is (in-high-schoolp (build 'student)))
  (is (not (in-high-schoolp
            (build 'student
                   :grade-level 8)))))

(test is-failing
  (let ((student (build 'poor-student)))
    (is (not (is-passingp student)))
    (is (= 0.5
           (amount-to-pass student)))))
