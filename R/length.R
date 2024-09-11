#' Check if an argument has a specific length
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_length()` check if an argument has a specific, minimum, or maximum length.
#'
#' @param x An R object.
#' @param len (optional) an integer number indicating the expected length of `x`
#'   (default: `NULL`).
#' @param min_len (optional) an integer number indicating the minimum length of
#'   `x` (default: `NULL`).
#' @param max_len (optional) an integer number indicating the maximum length of
#'   `x` (default: `NULL`).
#' @param null_ok (optional) a [`logical`][base::as.logical()] flag indicating
#'   if `x` can be `NULL` (default: `FALSE`).
#'
#' @template return_a
#' @include make_check.R
#' @export
#'
#' @examples
#' test_length(1, len = 1)
#' #> [1] TRUE # Expected
test_length <- function(
    x,
    len = NULL,
    min_len = NULL,
    max_len = NULL,
    null_ok = FALSE
  ) {
  if (isTRUE(test_first_check_family())) {
    assert_integer_number(len, lower = 0, null.ok = TRUE)
    assert_integer_number(min_len, lower = 0, null.ok = TRUE)
    assert_integer_number(max_len, lower = 0, null.ok = TRUE)
    assert_pick(len, c(min_len, max_len), pick = 1)
    assert_null_ok(x, null_ok)
  }

  if (!is.null(len)) {
    length(x) == len
  } else if (!is.null(min_len) && is.null(max_len)) {
    length(x) >= min_len
  } else if (is.null(min_len) && !is.null(max_len)) {
    length(x) <= max_len
  } else if (!is.null(min_len) && !is.null(max_len)) {
    length(x) >= min_len && length(x) <= max_len
  } else {
    else_error()
  }
}

message_length <- function(
    x,
    len = NULL,
    min_len = NULL,
    max_len = NULL,
    null_ok = FALSE,
    names
  ) {
  if (isTRUE(test_first_check_family())) {
    assert_integer_number(len, lower = 0, null.ok = TRUE)
    assert_integer_number(min_len, lower = 0, null.ok = TRUE)
    assert_integer_number(max_len, lower = 0, null.ok = TRUE)
    assert_pick(len, c(min_len, max_len), pick = 1)
    assert_string(names)
  }

  names <- collapse_names(color = "red", names = names)

  if (!is.null(max_len)) if (max_len == 0) len <- 0

  if (!is.null(len)) {
    glue::glue(
      "{names} must have {{.strong {len}}} {{cli::qty(len)}} element{{?s}}."
    )
  } else if (!is.null(min_len) && length(x) < min_len) {
    glue::glue("{names} must have {{.strong {min_len}}} or more elements.")
  } else if (!is.null(max_len) && length(x) > max_len) {
    glue::glue("{names} must have {{.strong {max_len}}} or less elements.")
  } else {
    else_error()
  }
}

#' @rdname test_length
#' @export
check_length <- make_check("check", "length", "x")

#' @rdname test_length
#' @export
assert_length <- make_check("assert", "length", "x")
