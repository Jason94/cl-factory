(in-package :cl-factory)

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

(defun drop-property (plist)
  "Drop the first property (key/value pair) from a plist."
  (rest (rest plist)))

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
                     (recur (drop-property orig-plist)
                            (append (subseq orig-plist 0 2)
                                    new-plist))
                     (recur (drop-property orig-plist)
                            new-plist)))))
    (reverse-plist (recur plist '()))))
