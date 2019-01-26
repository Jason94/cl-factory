# Roadmap

The current version of cl-factory is 0.1.0. This roadmap outlines our currently planned features and milestones. If you'd like to contribute but don't know where to start, try one of these!

## Future Releases

### 0.2.0 - Arguments Expansion

- [X] Static arguments (arguments that are evaluated at factory-definition time, not build time)
- [ ] Dependent arguments (arguments that reference other arguments)
- [ ] Transient arguments (arguments which are used for dependent arguments, but aren't injected into the object)
- [ ] Submit to Quicklisp

### 0.3.0 - Factories Expansion

- [ ] Factory inheritance (Nest factories or specify as an attribute? Both?)
- [ ] Sequence arguments (Support generation of unique values across different builds and different factories)
-  - [ ] Expose `(defgeneric next-in-sequence)`, and allow sequences to specify an arbitrary starting value, so that sequences can be built on any type.

### 0.4.0
- [ ] Struct factories
- [ ] Fiveam integration
