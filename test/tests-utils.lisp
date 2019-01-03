(in-package :cl-factory-test)

(def-suite utils-suite
    :description "Test the utilities the main factory code relies on."
    :in all-factory-tests)

(in-suite utils-suite)

(test take-n--empty
  "Taking n'th element of the empty list is an empty list."
  (is (equal '()
             (cl-factory::take-n '() 2))))

(test take-n--full
  "Taking n'th element of a full list returns only those elements."
  (is (equal '(1 1 1)
             (cl-factory::take-n '(1 2 3 1 2 3 1 2 3) 3))))

(test property-position--empty
  "The position of a key in an empty plist is nil."
  (is (equal nil
             (cl-factory::property-position '() :x))))

(test property-position--missing
  "The position of a key absent in a plist is nil."
  (is (equal nil
             (cl-factory::property-position '(:a 1 :b 2 :c 3) :x))))

(test property-position--present
  "The position of a key present in a plist is its position in the original list."
  (is (equal 2
             (cl-factory::property-position '(:a 1 :b 2 :c 3) :b)))
  (is (equal 4
             (cl-factory::property-position '(:a 1 :b 2 :c 3) :c))))

(test drop-property--empty
  "Dropping a property from the empty plist returns the empty plist."
  (is (equal '()
             (cl-factory::drop-property '()))))

(test drop-property--full
  "Dropping a plist from a full plist drops the key and the value."
  (is (equal '(:b 2 :c 3)
             (cl-factory::drop-property '(:a 1 :b 2 :c 3)))))

(test reverse-plist--empty
  "Reversing the empty plist returns the empty plist."
  (is (equal '()
             (cl-factory::reverse-plist '()))))

(test reverse-plist--full
  "Reversing a full plist maintains the key/value pairs."
  (is (equal '(:c 3 :b 2 :a 1)
             (cl-factory::reverse-plist '(:a 1 :b 2 :c 3)))))

(test clean-plist--empty
  "Cleaning the empty plist returns the empty plist."
  (is (equal '()
             (cl-factory::clean-plist '()))))

(test clean-plist--no-dups
  "Cleaning a plist with no duplicates returns that plist."
  (is (equal '(:a 1 :b 2 :c 3)
             (cl-factory::clean-plist '(:a 1 :b 2 :c 3)))))

(test clean-plist--with-adjacent-dups
  "Cleaning a plist with adjacent duplicates removes one of them."
  (is (equal  '(:a 1 :b 2 :c 3)
              (cl-factory::clean-plist '(:a 1 :a 1 :b 2 :c 3 :c 3)))))

(test clean-plist--with-separated-dups
  "Cleaning a plist with duplicates not adjacent keeps the first one in place."
  (is (equal  '(:a 1 :b 2 :c 3)
             (cl-factory::clean-plist '(:a 1 :b 2 :a 1 :c 3 :a 1 :c 3)))))
