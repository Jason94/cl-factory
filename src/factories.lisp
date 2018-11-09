(in-package :cl-factory)

(declaim (optimize (speed 0) (safety 0) (debug 3)))

;;; Utils
(defun is-quoted (maybe-quoted)
  "Check if a form is a quote form (quote XXX)"
  (and (consp maybe-quoted)
       (equalp 'quote
               (first maybe-quoted))
       (= 2 (length maybe-quoted))))

(defun ensure-list (maybe-list)
  "Convert an arg to a list if it's not."
  (if (consp maybe-list)
      (if (is-quoted maybe-list)
          (list maybe-list)
          maybe-list)
      (list maybe-list)))

(defun ensure-quoted (maybe-quoted)
  "Coerce a form to the form (quote XXX)"
  (cond
    ((is-quoted maybe-quoted) maybe-quoted)
    ((symbolp maybe-quoted) `(quote ,maybe-quoted))
    (t nil)))

(defun ensure-symbol (maybe-symbol)
  "Convert the form you get when you quote a symbol in a macro-arg
   to a symbol."
  (cond
    ((symbolp maybe-symbol) maybe-symbol)
    ((is-quoted maybe-symbol) (second maybe-symbol))
    (t nil)))

(defun take-n (list n)
  "Return a list taking every nth element of list."
  (loop for i from 0 to (1- (length list)) by n
        collecting (nth i list)))

(defun property-position (plist property)
  "Return the nth place of property in plist, or nil."
  (let ((i-property (position property (take-n plist 2) :test #'equal)))
    (when i-property
      (* 2 i-property))))

(defun reverse-plist (plist)
  "Reverse the order of the key/value pairs in a plist."
  (loop for i from (1- (length plist)) downto 0 by 2
        appending (subseq plist (1- i) (1+ i))))

(defun clean-plist (plist)
  "Remove duplicate key-entries from a plist"
  (labels ((recur (orig-plist new-plist)
             (if (not orig-plist)
                 new-plist
                 (if (not (property-position new-plist (first orig-plist)))
                     (recur (rest (rest orig-plist))
                            (append (subseq orig-plist 0 2)
                                    new-plist))
                     (recur (rest (rest orig-plist))
                            new-plist)))))
    (reverse-plist (recur plist '()))))

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
    :documentation "The form to evaluate and supply to the constructory.")))

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
    (if (find-class (class-symbol factory) nil)
        (apply #'make-instance
               (class-symbol factory)
               args-plist)
        args-plist)))
