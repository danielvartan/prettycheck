# `*_numeric()` was created as a workaround to deal with cases like
# `is.numeric(lubridate::duration())`. See
# https://github.com/tidyverse/lubridate/issues/942 to learn more.

#' @include make_check.R
NULL

test_numeric <- function(
    x,
    lower = -Inf,
    upper = Inf,
    any.missing = TRUE,
    null_ok = FALSE) {
  assert_number(lower)
  assert_number(upper)
  assert_flag(any.missing)
  assert_flag(null_ok)

  classes <- c("integer", "double", "numeric")

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any.missing)) {
    FALSE
  } else if (test_subset(class(x)[1], classes) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
    FALSE
  } else {
    test_subset(class(x)[1], classes)
  }
}

check_numeric <- function(
    x,
    lower = - Inf,
    upper = Inf,
    any.missing = TRUE,
    null_ok = FALSE,
    name = deparse(substitute(x))
  ) {
  assert_flag(any.missing)
  assert_flag(null_ok)

  classes <- c("integer", "double", "numeric")

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any.missing)) {
    paste0(glue::single_quote(name), " cannot have missing values")
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(name), " cannot be 'NULL'")
  } else if (test_subset(class(x)[1], classes) &&
               !all(x >= lower, na.rm = TRUE)) {
    paste0("Element ", which(x < lower)[1], " is not >= ", lower)
  } else if (test_subset(class(x)[1], classes) &&
               !all(x <= upper, na.rm = TRUE)) {
    paste0("Element ", which(x > upper)[1], " is not <= ", upper)
  } else  if (!test_numeric(x)) {
    paste0("Must be of type 'numeric', not ", class_collapse(x))
  } else {
    TRUE
  }
}

assert_numeric <- checkmate::makeAssertionFunction(check_numeric)

# This is temporary!

expect_numeric <- checkmate::expect_numeric
