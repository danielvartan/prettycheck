#' @include make_check.R
NULL

# Check checkmate::assert_count()

test_whole_number <- function(
    x,
    any.missing = TRUE,
    null_ok = FALSE,
    tol = .Machine$double.eps^0.5
  ) {
  assert_flag(any.missing)
  assert_flag(null_ok)
  assert_number(tol)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any.missing)) {
    FALSE
  } else if (!test_numeric(x) || !identical(x, abs(x))) {
    FALSE
  } else {
    all(abs(x - round(x)) < tol, na.rm = any.missing)
  }
}

check_whole_number <- function(x, any.missing = TRUE, null_ok = FALSE,
                               name = deparse(substitute(x))) {
  assert_flag(any.missing)
  assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any.missing)) {
    paste0(glue::single_quote(name), " cannot have missing values")
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(name), " cannot be 'NULL'")
  } else  if (!test_whole_number(x)) {
    paste0(glue::single_quote(name), " must consist of whole numbers")
  } else {
    TRUE
  }
}

assert_whole_number <- checkmate::makeAssertionFunction(check_whole_number)

# expect_whole_number
