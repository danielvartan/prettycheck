#' @include make_check.R
NULL

# This is temporary!

test_false <- checkmate::test_false
check_false <- checkmate::check_false
assert_false <- checkmate::assert_false

expect_false <- checkmate::makeExpectationFunction(
  checkmate::check_false,
  use.namespace = TRUE
)
