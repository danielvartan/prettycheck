#' Check if an argument is a [`Period`][lubridate::period] object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_period()` check if an argument is a
#' [`lubridate`](https://lubridate.tidyverse.org/)
#' [`Period`][lubridate::period] object.
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
#' test_period(1)
#' #> [1] FALSE # Expected
#'
#' test_period(hms::parse_hm("01:00"))
#' #> [1] FALSE # Expected
#'
#' test_period(lubridate::hours())
#' #> [1] TRUE # Expected
#'
#' test_period(lubridate::hours(1), lower = lubridate::hours(2))
#' #> [1] FALSE # Expected
#'
#' test_period(lubridate::hours(2), upper = lubridate::hours(1))
#' #> [1] FALSE # Expected
#'
#' test_period(c(lubridate::hours(), NA), any_missing = FALSE)
#' #> [1] FALSE # Expected
#'
#' test_period(NULL, null_ok = FALSE)
#' #> [1] FALSE # Expected
check_period <- function( #nolint
    x, #nolint
    lower = - Inf,
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
  } else if (lubridate::is.period(x) && !all(x >= lower, na.rm = TRUE)) {
    paste0("Element ", which(x < lower)[1], " is not >= ", lower)
  } else if (lubridate::is.period(x) && !all(x <= upper, na.rm = TRUE)) {
    paste0("Element ", which(x > upper)[1], " is not <= ", upper)
  } else  if (!test_period(x)) {
    paste0("Must be of type 'Period', not ", class_collapse(x))
  } else {
    TRUE
  }
}

#' @rdname check_period
#' @export
test_period <- function(
    x, #nolint
    lower = - Inf,
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
  } else if (lubridate::is.period(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
    FALSE
  } else {
    lubridate::is.period(x)
  }
}

#' @rdname check_period
#' @export
assert_period <- checkmate::makeAssertionFunction(check_period)

#' @rdname check_period
#' @export
expect_period <- checkmate::makeExpectationFunction(
  check_period,
  use.namespace = TRUE
)
