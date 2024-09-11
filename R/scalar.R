#' @include make_check.R
NULL

test_scalar <- function(x) length(x) == 1

check_scalar <- function(x, name = deparse(substitute(x))) {
  if (!(test_scalar(x))) {
    paste0(glue::single_quote(name), " must have length 1, not length ",
           length(x))
  } else {
    TRUE
  }
}

assert_scalar <- checkmate::makeAssertionFunction(check_scalar)

expect_scalar <- checkmate::expect_scalar
