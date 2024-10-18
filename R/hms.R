test_hms <- function(x, lower = - Inf, upper = Inf, any_missing = TRUE,
                     null_ok = FALSE) {
  assert_flag(any_missing)
  assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    FALSE
  } else if (hms::is_hms(x) && !all(x >= lower & x <= upper, na.rm = TRUE)) {
    FALSE
  } else {
    hms::is_hms(x)
  }
}

check_hms <- function(x, lower = - Inf, upper = Inf, any_missing = TRUE,
                      null_ok = FALSE,
                      name = deparse(substitute(x))) {
  assert_flag(any_missing)
  assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    paste0(glue::single_quote(name), " cannot have missing values")
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(name), " cannot be 'NULL'")
  } else if (hms::is_hms(x) && !all(x >= lower, na.rm = TRUE)) {
    paste0("Element ", which(x < lower)[1], " is not >= ", lower)
  } else if (hms::is_hms(x) && !all(x <= upper, na.rm = TRUE)) {
    paste0("Element ", which(x > upper)[1], " is not <= ", upper)
  } else  if (!test_hms(x)) {
    paste0("Must be of type 'hms', not ", class_collapse(x))
  } else {
    TRUE
  }
}

assert_hms <- checkmate::makeAssertionFunction(check_hms)

expect_hms <- checkmate::makeExpectationFunction(
  check_hms,
  use.namespace = TRUE
)
