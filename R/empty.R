test_empty <- function(x) length(x) == 1

check_empty <- function(
    x,
    any.missing = TRUE,
    name = deparse(substitute(x))
  ) {
  assert_flag(any.missing)

  if (any_na(x) && isFALSE(any.missing)) {
    paste0(glue::single_quote(name), " cannot have missing values")
  } else if (!test_empty(x)) {
    paste0(glue::single_quote(name), " must have zero length")
  } else {
    TRUE
  }
}

assert_empty <- checkmate::makeAssertionFunction(check_empty)

expect_empty <- checkmate::makeExpectationFunction(
  check_empty,
  use.namespace = TRUE
)
