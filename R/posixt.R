#' Check if an argument is a [`POSIXt`][base::as.POSIXct] object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_posixt()` check if an argument is a [`POSIXt`][base::as.POSIXct]
#' object.
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
#' test_posixt(1)
#' #> [1] FALSE # Expected
#'
#' test_posixt(hms::as_hms("01:02:03"))
#' #> [1] FALSE # Expected
#'
#' test_posixt(Sys.time())
#' #> [1] TRUE # Expected
#'
#' test_posixt(Sys.time(), lower = Sys.time() + lubridate::dhours(1))
#' #> [1] FALSE # Expected
#'
#' test_posixt(Sys.time(), upper = Sys.time() - lubridate::dhours(1))
#' #> [1] FALSE # Expected
#'
#' test_posixt(c(Sys.time(), NA), any_missing = FALSE)
#' #> [1] FALSE # Expected
#'
#' test_posixt(NULL, null_ok = FALSE)
#' #> [1] FALSE # Expected
check_posixt <- function( #nolint
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

#' @rdname check_posixt
#' @export
test_posixt <- function(
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
  } else if (lubridate::is.POSIXt(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
    FALSE
  } else {
    lubridate::is.POSIXt(x)
  }
}

#' @rdname check_posixt
#' @export
assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

#' @rdname check_posixt
#' @export
expect_posixt <- checkmate::makeExpectationFunction(
  check_posixt,
  use.namespace = TRUE
)
