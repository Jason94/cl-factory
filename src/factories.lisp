(in-package :cl-factory)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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

(defun func-wrap-args (args)
  (mapcar (lambda (x) (lambda () x))
          args))

(defmacro define-factory (class-symbol &body rest)
  `(let ((func-wrapped-args (func-wrap-args ,rest)))
    setf (gethash ,class-symbol *factories*) (list ,@rest)))

(defmacro build (class-symbol &rest args)
  (let* ((norm-class-symbol (ensure-symbol class-symbol))
         (default-slot-vals (get-factory norm-class-symbol))
         (all-slot-vals (append args default-slot-vals))
         (slots (get-plist-keys all-slot-vals))
         (merged-slot-vals (mapcar (lambda (slot)
                                     (getf all-slot-vals slot))
                                   slots))
         (make-instance-args (weave slots merged-slot-vals)))
    `(make-instance ,class-symbol ,@make-instance-args)))
