test_posixt <- function(
    x,
    lower = - Inf,
    upper = Inf,
    any.missing = TRUE,
    null_ok = FALSE
  ) {
  assert_flag(any.missing)
  assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any.missing)) {
    FALSE
  } else if (lubridate::is.POSIXt(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
    FALSE
  } else {
    lubridate::is.POSIXt(x)
  }
}

check_posixt <- function(
    x,
    lower = - Inf,
    upper = Inf,
    any.missing = TRUE,
    null_ok = FALSE,
    name = deparse(substitute(x))
  ) {
  assert_flag(any.missing)
  assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any.missing)) {
    paste0(glue::single_quote(name), " cannot have missing values")
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(name), " cannot be 'NULL'")
  } else if (lubridate::is.POSIXt(x) && !all(x >= lower, na.rm = TRUE)) {
    paste0("Element ", which(x < lower)[1], " is not >= ", lower)
  } else if (lubridate::is.POSIXt(x) && !all(x <= upper, na.rm = TRUE)) {
    paste0("Element ", which(x > upper)[1], " is not <= ", upper)
  } else  if (!lubridate::is.POSIXt(x)) {
    paste0(
      "Must be of type 'POSIXct' or 'POSIXlt', not ",
      class_collapse(x)
    )
  } else {
    TRUE
  }
}

assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

# expect_posixt
