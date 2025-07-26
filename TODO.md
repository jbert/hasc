- Need to handle open/close parens in the middle of a symbol

- Need tests for these:
  (
  )
  )(
  (()
  -- Not an error, we don't require w/s around parens
  foo(

- have parse initially be a single expression

- then parse to a list of expressions
  - wrapped in a `(do ...)` or similar

- add some parsing tests
  - do round-trip to string for ease

- implement eval

- use Either instead of error for Callable?
