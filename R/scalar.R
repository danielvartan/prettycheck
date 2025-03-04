#' Check if an argument is a scalar object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_scalar()` check if an argument is a
#' [scalar](https://intro2r.com/data-structures.html#scal_vecs) object.
#'
#' @template param-x
#' @template param-null_ok
#' @template param-.names
#' @template param-checkmate-minus-.var_name
#' @export
#'
#' @examples
#' test_scalar(1)
#' #> [1] TRUE # Expected
#'
#' test_scalar(1:2)
#' #> [1] FALSE # Expected
#'
#' test_scalar(list())
#' #> [1] FALSE # Expected
check_scalar <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(.names), " cannot be 'NULL'")
  } else if (isFALSE(test_scalar(x))) {
    paste0(glue::single_quote(.names), " is not a scalar object.")
  } else {
    TRUE
  }
}

#' @rdname check_scalar
#' @export
test_scalar <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (is.null(x) && isFALSE(null_ok)) {
    FALSE
  } else if (test_atomic(x) && test_length(x, len = 1)) {
    TRUE
  } else {
    FALSE
  }
}

#' @rdname check_scalar
#' @export
assert_scalar <- checkmate::makeAssertionFunction(check_scalar)

#' @rdname check_scalar
#' @export
expect_scalar <- checkmate::makeExpectationFunction(
  check_scalar,
  use.namespace = TRUE
)
