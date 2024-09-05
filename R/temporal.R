test_temporal <- function(x, any.missing = TRUE, null_ok = FALSE, rm = NULL) {
  assert_flag(any.missing)
  assert_flag(null_ok)
  assert_character(rm, any.missing = FALSE, null.ok = TRUE)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any.missing)) {
    FALSE
  } else {
    classes <- c("Duration", "Period", "difftime", "hms", "Date", "POSIXct",
                 "POSIXlt", "Interval")

    if (!is.null(rm)) {
      rm <- paste0("^", rm, "$", collapse = "|")
      classes <- stringr::str_subset(classes, rm, negate = TRUE)
    }

    test_subset(class(x)[1], classes)
  }
}

check_temporal <- function(
    x,
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
  } else if (!test_temporal(x)) {
    paste0("Must be a temporal object (see 'test_temporal()'), ",
           "not ", class_collapse(x))
  } else {
    TRUE
  }
}

assert_temporal <- checkmate::makeAssertionFunction(check_temporal)

# expect_temporal
