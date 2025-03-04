#' Check if a namespace is loaded
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_namespace()` check if an argument has zero length.
#'
#' @template param-x
#' @template param-null_ok
#' @template param-.names
#' @template param-checkmate-minus-.var_name
#' @export
#'
#' @examples
#' test_namespace("TeSt")
#' #> [1] FALSE # Expected
#'
#' test_namespace("prettycheck")
#' #> [1] TRUE # Expected
check_namespace <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_string(x)
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(.names), " cannot be 'NULL'")
  } else if (isFALSE(test_namespace(x))) {
    paste0(glue::single_quote(x), " namespace is not loaded.")
  } else {
    TRUE
  }
}


#' @rdname check_namespace
#' @export
test_namespace <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_string(x)
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (is.null(x) && isFALSE(null_ok)) {
    FALSE
  } else {
    isNamespaceLoaded(x)
  }
}

#' @rdname check_namespace
#' @export
assert_namespace <- checkmate::makeAssertionFunction(check_namespace)

#' @rdname check_namespace
#' @export
expect_namespace <- checkmate::makeExpectationFunction(
  check_namespace,
  use.namespace = TRUE
)
