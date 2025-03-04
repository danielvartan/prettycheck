#' Check if an argument is a [`Duration`][lubridate::duration] object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_duration()` check if an argument is a
#' [`lubridate`](https://lubridate.tidyverse.org/)
#' [`Duration`][lubridate::duration] object.
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
#' test_duration(1)
#' #> [1] FALSE # Expected
#'
#' test_duration(hms::parse_hm("01:00"))
#' #> [1] FALSE # Expected
#'
#' test_duration(lubridate::dhours())
#' #> [1] TRUE # Expected
#'
#' test_duration(lubridate::dhours(1), lower = lubridate::dhours(2))
#' #> [1] FALSE # Expected
#'
#' test_duration(lubridate::dhours(2), upper = lubridate::dhours(1))
#' #> [1] FALSE # Expected
#'
#' test_duration(c(lubridate::dhours(), NA), any_missing = FALSE)
#' #> [1] FALSE # Expected
#'
#' test_duration(NULL, null_ok = FALSE)
#' #> [1] FALSE # Expected
check_duration <- function( #nolint
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
  } else if (lubridate::is.duration(x) && !all(x >= lower, na.rm = TRUE)) {
    paste0("Element ", which(x < lower)[1], " is not >= ", lower)
  } else if (lubridate::is.duration(x) && !all(x <= upper, na.rm = TRUE)) {
    paste0("Element ", which(x > upper)[1], " is not <= ", upper)
  } else  if (!test_duration(x)) {
    paste0("Must be of type 'Duration', not ", class_collapse(x))
  } else {
    TRUE
  }
}

#' @rdname check_duration
#' @export
test_duration <- function(
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
  } else if (lubridate::is.duration(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
    FALSE
  } else {
    lubridate::is.duration(x)
  }
}

#' @rdname check_duration
#' @export
assert_duration <- checkmate::makeAssertionFunction(check_duration)

#' @rdname check_duration
#' @export
expect_duration <- checkmate::makeExpectationFunction(
  check_duration,
  use.namespace = TRUE
)
