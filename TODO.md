- Need to handle open/close parens in the middle of a symbol

- Need tests for these:
  (
  )
  )(
  (()
  -- Not an error, we don't require w/s around parens
  foo(
