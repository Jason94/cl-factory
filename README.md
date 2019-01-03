cl-factory is a library for simplifying repetitive creation of varius Common Lisp data structures. The main usage of such a library is making your test suites shorter, more expressive, and easier to maintain.

(Credit to [factory_bot](https://www.rubydoc.info/gems/factory_bot/file/GETTING_STARTED.md), which *heavily* inspired most of the cl-factory API)

## Data types

cl-factory currently supports creating these structures:

- CLOS objects
- plists

We will eventually support other common structures as well. Please see [roadmap](https://github.com/Jason94/cl-factory/blob/master/ROADMAP.md) for more information on planned features.


## Defining a new factory

You can define a new factory by using `define-factory` like this:
```
(define-factory 'student
  :name "John Doe"
  :gpa  3.5)
```
`define-factory` will look for a CLOS class with the given symbol-name. If it can find such a class, it will define a factory for that class. If it cannot, it will create a plist with the given keys & values.

## Constructing an object

Once a factory has been defined, you can use it to instantiate an object using `build`. Exactly what `build` produces will depend on the kind of factory you are building.
```
(build 'student)

CLOS: (make-instance 'student :name "John Doe" :gpa 3.5)
plist: (list :name "John Doe" :gpa 3.5)
```

You can add extra arguments to a build:
```
(build 'student :teacher "Mrs. White")
```

And you can override parameters in the same way:
```
(build 'student :name "Jane Doe")
```


## Named factories

Factories can be given an alias:
```
(define-factory ('bad-student :class 'student)
  :name "Failing Frankie"
  :gpa 2.0)
```
This factory is run by calling `(build 'bad-student)`. 

Note that plist factories don't need aliases. Just use
```
(define-factory 'bad-student
  ...)
```

## Arguments as expressions

Arguments to factories are evaluated as Lisp expressions, so you can define factory arguments like this:
```
(define-factory ('average-student :class 'student)
  :name "Average Annie"
  :gpa (average *all-student-gpas*))
``` 
Arguments are evaluated at build time, not at factory-definition, so be careful when referencing mutable values from factory definitions:
```
(defvar *value* 0)

(define-factory 'numbers
  :zero *value*
  :one  (1+ *value*))

(setf *value* 1)

(build 'numbers) ;; Returns: '(:zero 1 :one 2)
```