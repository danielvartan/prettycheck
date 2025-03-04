#' Check if an argument has zero length
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_empty()` check if an argument has zero length.
#'
#' @template param-x
#' @template param-null_ok
#' @template param-.names
#' @include length.R
#' @export
#'
#' @examples
#' test_empty(1)
#' #> [1] FALSE # Expected
#'
#' test_empty(character())
#' #> [1] TRUE # Expected
check_empty <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  check_length(x, len = 0, null_ok = null_ok, .names = .names)
}

#' @rdname check_empty
#' @export
test_empty <- function(
  x, #nolint
  null_ok = FALSE,
  .names = deparse(substitute(x))
) {
  test_length(x, len = 0, null_ok = null_ok, .names = .names)
}

#' @rdname check_empty
#' @export
assert_empty <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_length(x, len = 0, null_ok = null_ok, .names = .names)
}

# expect_empty
