(in-package :cl-factory)

(declaim (optimize (speed 0) (safety 0) (debug 3)))

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
    :documentation "The form to evaluate and supply to the constructor.")))

(defmethod print-object ((slot-arg slot-arg) stream)
  (print-unreadable-object (slot-arg stream :type t :identity t)
    (format stream "Key: ~a | Form: ~a" (key slot-arg) (form slot-arg))))

(defun plist-to-slot-args (plist)
  "Convert a plist of the form (:key form) to a list of slot-arg."
  (loop for i from 0 to (1- (length plist)) by 2
        for key = (nth i plist)
        for form = (nth (1+ i) plist)
        collecting (make-instance 'slot-arg :key key :form form)))

(defun slot-args-to-plist (slot-args)
  "Convert a list of slot-args to a plist of their keys and evaluated forms."
  (mapcan (lambda (slot-arg)
            (list (key slot-arg) (eval (form slot-arg))))
          slot-args))

(defun slot-args-to-make-instance (class-symbol slot-args)
  (let ((args-plist (slot-args-to-plist slot-args)))
    (apply #'make-instance class-symbol args-plist)))

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
   args - class-symbol OR (class-symbol &key alias)
   TODO: Improve this doc"
  (let* ((args-list (ensure-list args))
         (class-symbol (args-to-class-sym args-list))
         (alias (args-to-alias args-list))
         (reference (or alias class-symbol)))
    `(setf (gethash ,reference *factories*)
           (make-instance 'factory
                          :class-symbol ,class-symbol
                          :alias ,alias
                          :slot-args (plist-to-slot-args ',rest)))))

(defun build (factory-name &rest args)
  "Build an instance of a factory.
   TODO: Improve this doc."
  (let* ((factory (get-factory (ensure-symbol factory-name)))
         (args-plist (clean-plist
                      (append
                       args
                       (slot-args-to-plist (slot-args factory))))))
    (cond
      ((find-class (class-symbol factory) nil) (apply #'make-instance
                                                      (class-symbol factory)
                                                      args-plist))
       (t args-plist))))
