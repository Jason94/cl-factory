# Contributing

Thank you for helping cl-factory! This document will help you find a way to contribute if you aren't sure how, and give a few recommendations for your pull requests.

## What should I do?

First, if there is a feature that you think would be helpful for users, or would like to use yourself, we would love to have it! Otherwise, take a look at our [issues](https://github.com/Jason94/cl-factory/issues) and [roadmap](https://github.com/Jason94/cl-factory/blob/master/ROADMAP.md). The roadmap gives an outline of planned future releases, what we've finished so far, and what we haven't. 

## Pull Request Guidelines

1. ### Tests pass
   Making sure the existing tests all pass is the most important thing you can do to help your pull request get merged. If the tests don't pass, you'll probably be asked to fix this before the PR is merged. To run the tests you can use the command `(asdf:test-system "cl-factory")` in a Common Lisp repl.
2. ### Writing new tests
   Any new features should be covered by tests. 100% code coverage by the tests is not necessary. Ideally, though, any breaking change in the code should trigger at least one test failure.
3. ### Atomic commits
   Each commit should focus on one concern to make the pull request and repository history easier to read.