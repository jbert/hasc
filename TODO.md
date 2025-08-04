
- DONE Have `Atom` take a `Val`, not a `Token`

- DONE add parse tests

- DONE Add special forms (if, do)

- DONE Add lambda

- add error for mismatch between number of lambda args and body

- DONE Add let

- add let* (or whichever one binds-as-it-goes)
  - rewrite to nested let?

- add error for wrongly-structured let bindings list

- share code for Primitive and ELambda in eval

- add eval tests
  - add tests for lambda eval
  - include environment capture

- add print

- add implicit 'do' to lambda

- add top-level define

- add lambda 'define' syntactic sugar
  - as primitive or code transformation?

- add parse tests for if / do
  
- add some basic library funcs

- work out "whole file" semantics
  - top-level define
  - implicit 'do'?
