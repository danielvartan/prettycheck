#' Check if an argument has length greater than zero
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_not_empty()` check if an argument has length greater than zero.
#'
#' @template param-x
#' @template param-null_ok
#' @template param-.names
#' @include length.R
#' @export
#'
#' @examples
#' test_not_empty(character())
#' #> [1] FALSE # Expected
#'
#' test_not_empty(1)
#' #> [1] TRUE # Expected
check_not_empty <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  check_length(x, min_len = 1, null_ok = null_ok, .names = .names)
}

#' @rdname check_not_empty
#' @export
test_not_empty <- function(
  x, #nolint
  null_ok = FALSE,
  .names = deparse(substitute(x))
) {
  test_length(x, min_len = 1, null_ok = null_ok, .names = .names)
}

#' @rdname check_not_empty
#' @export
assert_not_empty <- function(
    x, #nolint
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_length(x, min_len = 1, null_ok = null_ok, .names = .names)
}

# expect_not_empty
