#' Check if an argument is an [`Interval`][lubridate::interval] object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_interval()` check if an argument is a
#' [`lubridate`](https://lubridate.tidyverse.org/)
#' [`Interval`][lubridate::interval] object.
#'
#' @template param-x
#' @template param-lower_upper
#' @template param-any_missing
#' @template param-null_ok
#' @template param-.names
#' @template param-checkmate-minus-.var_name
#' @template return-default
#' @export
#'
#' @examples
#' test_interval(1)
#' #> [1] FALSE # Expected
#'
#' test_interval(lubridate::dhours())
#' #> [1] FALSE # Expected
#'
#' int <- lubridate::interval(as.Date("2001-01-01"), as.Date("2002-01-01"))
#'
#' test_interval(int)
#' #> [1] TRUE # Expected
#'
#' test_interval(int, lower = lubridate::dyears(2))
#' #> [1] FALSE # Expected
#'
#' test_interval(int, upper = lubridate::dyears(0.5))
#' #> [1] FALSE # Expected
#'
#' test_interval(c(int, NA), any_missing = FALSE)
#' #> [1] FALSE # Expected
#'
#' test_interval(NULL, null_ok = FALSE)
#' #> [1] FALSE # Expected
check_interval <- function( #nolint
    x, #nolint
    lower = -Inf,
    upper = Inf,
    any_missing = TRUE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_number(lower)
  assert_number(upper)
  assert_flag(any_missing)
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    paste0(glue::single_quote(.names), " cannot have missing values")
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(.names), " cannot be 'NULL'")
  } else if (lubridate::is.interval(x) && !all(x >= lower, na.rm = TRUE)) {
    paste0("Element ", which(x < lower)[1], " is not >= ", lower)
  } else if (lubridate::is.interval(x) && !all(x <= upper, na.rm = TRUE)) {
    paste0("Element ", which(x > upper)[1], " is not <= ", upper)
  } else  if (!lubridate::is.interval(x)) {
    paste0("Must be of type 'Interval', not ", class_collapse(x))
  } else {
    TRUE
  }
}

#' @rdname check_interval
#' @export
test_interval <- function(
    x, #nolint
    lower = -Inf,
    upper = Inf,
    any_missing = TRUE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_number(lower)
  assert_number(upper)
  assert_flag(any_missing)
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    FALSE
  } else if (lubridate::is.interval(x) &&
             !all(x >= lower & x <= upper, na.rm = TRUE)) { #nolint
    FALSE
  } else {
    lubridate::is.interval(x)
  }
}

#' @rdname check_interval
#' @export
assert_interval <- checkmate::makeAssertionFunction(check_interval)

#' @rdname check_interval
#' @export
expect_interval <- checkmate::makeExpectationFunction(
  check_interval,
  use.namespace = TRUE
)
