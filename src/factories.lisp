(in-package :cl-factory)

(declaim (optimize (speed 0) (safety 0) (debug 3)))

;;; Utils
(defun ensure-symbol (maybe-symbol)
  "Convert the form you get when you quote a symbol in a macro-arg
   to a symbol"
  (if (not (symbolp maybe-symbol))
      (second maybe-symbol)
      maybe-symbol))

(defun get-plist-keys (plist)
  (let ((keys '()))
    (loop for i from 0 to (1- (length plist)) by 2
          do (setf keys (adjoin (nth i plist)
                                keys)))
    (reverse keys)))

(defun weave (seq-a seq-b)
  (loop for a in seq-a
        for b in seq-b
     appending (list a b)))

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
    :documentation "The form to evaluate and supply to the constructory")))

(defmethod print-object ((slot-arg slot-arg) stream)
  (print-unreadable-object (slot-arg stream :type t :identity t)
    (format stream "Key: ~a | Form: ~a" (key slot-arg) (form slot-arg))))

(defun eval-form (slot-arg)
  (eval (form slot-arg)))

(defun plist-to-slot-args (plist)
  "Convert a plist of the form (:key form) to a list of slot-arg."
  (loop for i from 0 to (1- (length plist)) by 2
        for key = (nth i plist)
        for form = (nth (1+ i) plist)
        collecting (make-instance 'slot-arg :key key :form form)))

(defun slot-args-to-make-instance (class-symbol slot-args)
  (let* ((args-plist (loop for slot-arg in slot-args
                           for key = (key slot-arg)
                           for form = (form slot-arg)
                           appending `(,key (eval ,form))))
         (form `(make-instance ',class-symbol ,@args-plist)))
    (eval form)))

;;; Factories

(defvar *factories* (make-hash-table :test #'equal))

(defun factories-keys ()
  (loop for k being the hash-keys in *factories*
        collecting k))

(defun clear-factories ()
  (setf *factories* (make-hash-table :test #'equal)))

(defun get-factory (class-symbol)
  (gethash class-symbol *factories*))

(defun print-factories ()
  (format t "~&===============================~%")
  (loop for key in (factories-keys)
        do (format t "~a: ~a~%" key (get-factory key))))

(defmacro define-factory (class-symbol &body rest)
  `(setf (gethash ,class-symbol *factories*) (plist-to-slot-args ',rest)))

(defmacro build (class-symbol &rest args)
  (let* ((norm-class-symbol (ensure-symbol class-symbol))
         (default-slot-args (get-factory norm-class-symbol))
         (all-slot-args (append (plist-to-slot-args args) default-slot-args))
         (merged-slot-args
          (remove-duplicates all-slot-args :key #'key :from-end t)))
    `(slot-args-to-make-instance ,class-symbol (list ,@merged-slot-args))))
