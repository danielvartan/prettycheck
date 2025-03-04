#' Check if an argument is a [`hms`][hms::hms] object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_hms()` check if an argument is a [`hms`][lubridate::duration] object.
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
#' test_hms(1)
#' #> [1] FALSE # Expected
#'
#' test_hms(lubridate::dhours(1))
#' #> [1] FALSE # Expected
#'
#' test_hms(hms::parse_hm("01:00"))
#' #> [1] TRUE # Expected
#'
#' test_hms(hms::parse_hm("01:00"), lower = hms::parse_hm("02:00"))
#' #> [1] FALSE # Expected
#'
#' test_hms(hms::parse_hm("02:00"), upper = hms::parse_hm("01:00"))
#' #> [1] FALSE # Expected
#'
#' test_hms(c(hms::parse_hm("01:00"), NA), any_missing = FALSE)
#' #> [1] FALSE # Expected
#'
#' test_hms(NULL, null_ok = FALSE)
#' #> [1] FALSE # Expected
check_hms <- function( #nolint
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

#' @rdname check_hms
#' @export
test_hms <- function(
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
  } else if (hms::is_hms(x) && !all(x >= lower & x <= upper, na.rm = TRUE)) {
    FALSE
  } else {
    hms::is_hms(x)
  }
}

#' @rdname check_hms
#' @export
assert_hms <- checkmate::makeAssertionFunction(check_hms)

#' @rdname check_hms
#' @export
expect_hms <- checkmate::makeExpectationFunction(
  check_hms,
  use.namespace = TRUE
)
