(in-package :cl-factory)

;;; Factory
(defclass factory ()
  ((class-symbol
    :initarg :class-symbol
    :initform (error "Must supply a class-symbol")
    :reader class-symbol
    :documentation "Symbol for the class this factory constructs.")
   (alias
    :initarg :alias
    :initform nil
    :reader alias
    :documentation "Alias used to build this factory.")
   (slot-args
    :initarg :slot-args
    :initform '()
    :reader slot-args
    :documentation "List of slot-args to supply to #'make-instance")))

(defmethod print-object ((factory factory) stream)
  (print-unreadable-object (factory stream :type t :identity t)
    (format stream "Class: ~a | Alias: ~a" (class-symbol factory) (alias factory))))

;;; Args
(defclass slot-arg ()
  ((key
    :initarg :key
    :initform (error "Must supply a key")
    :reader key)
   (form
    :initarg :form
    :initform (error "Must supply a form")
    :reader form
    :documentation "The form to evaluate and supply to the constructor.")
   (is-static
    :initarg :is-static
    :initform nil
    :reader is-static)))

(defmethod print-object ((slot-arg slot-arg) stream)
  (print-unreadable-object (slot-arg stream :type t :identity t)
    (format stream "Key: ~a | Form: ~a" (key slot-arg) (form slot-arg))))

(defun factory-body-to-slot-args (body)
  "Convert a factory-definition body to a list of slot-arg. See define-factory
   for more documentation on the format."
  (let ((normalized-body (mapkeys #'ensure-list body)))
    (loop for i from 0 to (1- (length normalized-body)) by 2
          for slot-specifier = (nth i normalized-body)
          for key = (first slot-specifier)
          for is-static = (getf (rest slot-specifier) :static)
          for form = (nth (1+ i) normalized-body)
          for norm-form = (if is-static
                              (eval form)
                              form)
          collecting (make-instance 'slot-arg :key key :form norm-form :is-static is-static))))

(defun slot-args-to-evaluated-plist (slot-args)
  "Convert a list of slot-args to a plist of their keys and evaluated forms."
  (mapcan (lambda (slot-arg)
            (list (key slot-arg) (if (is-static slot-arg)
                                     (form slot-arg)
                                     (eval (form slot-arg)))))
          slot-args))

;;; Factories

(defvar *factories* (make-hash-table :test #'equalp)
  "Store a table of factories, keyed by their class-name or alias.")

(defun factories-keys ()
  "Get a list of all of the defined factories."
  (loop for k being the hash-keys in *factories*
        collecting k))

(defun clear-factories ()
  "Clear all of the factories."
  (setf *factories* (make-hash-table :test #'equalp)))

(defun get-factory (class-symbol)
  "Get the factory defined for a class-symbol."
  (gethash class-symbol *factories*))

(defun args-to-class-sym (args)
  (ensure-quoted
   (if (= 1 (length args))
       (first args)
       (or (getf (rest args) :class) ; plist starts after the ref-name
           (first args)))))

(defun args-to-alias (args)
  (when (and (> (length args) 1)
             (getf (rest args) :class)) ; plist starts after the ref-name
    (ensure-quoted (first args))))

(defmacro define-factory (args &body rest)
  "Define a new factory.
   args - class-symbol OR (alias &key class)
   rest - has the form :arg-name eval-form
                       (:arg-name ... arg plist) eval-form
   TODO: Improve this doc"
  (let* ((args-list (ensure-list args))
         (class-symbol (args-to-class-sym args-list))
         (alias (args-to-alias args-list))
         (reference (or alias class-symbol)))
    `(setf (gethash ,reference *factories*)
           (make-instance 'factory
                          :class-symbol ,class-symbol
                          :alias ,alias
                          :slot-args (factory-body-to-slot-args ',rest)))))

(defun build (factory-name &rest args)
  "Build an instance of a factory."
  (let* ((factory (get-factory (ensure-symbol factory-name)))
         (args-plist (clean-plist
                      (append
                       args
                       (slot-args-to-evaluated-plist (slot-args factory))))))
    (cond
      ((find-class (class-symbol factory) nil) (apply #'make-instance
                                                      (class-symbol factory)
                                                      args-plist))
       (t args-plist))))
