#' @include make_check.R
NULL

test_not_empty <- function(x) length(x) >= 1

check_not_empty <- function(
    x,
    any.missing = TRUE,
    name = deparse(substitute(x))
  ) {
  assert_flag(any.missing)

  if (any_na(x) && isFALSE(any.missing)) {
    paste0(glue::single_quote(name), " cannot have missing values")
  } else if (!test_not_empty(x)) {
    paste0(glue::single_quote(name), " must have length greater than zero")
  } else {
    invisible(TRUE)
  }
}

assert_not_empty <- checkmate::makeAssertionFunction(check_empty)

expect_not_empty <- checkmate::makeExpectationFunction(
  check_empty,
  use.namespace = TRUE
)
