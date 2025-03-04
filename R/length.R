#' Check if an argument has a specific length
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_length()` check if an argument has a specific, minimum, or maximum length.
#'
#' @param len (optional) an integer number indicating the expected length of `x`
#'   (default: `1`).
#'
#' @template param-x
#' @template param-max-min-len
#' @template param-null_ok
#' @template param-.names
#' @template return-default-minus-e
#' @include make_check.R
#' @export
#'
#' @examples
#' x <- 1:2
#'
#' test_length(x, len = 2)
#' #> [1] TRUE # Expected
#'
#' check_length(x, len = 1) |> cli::cli_alert_warning()
#' #> ! x must have 1 element. # Expected
#'
#' check_length(x, min_len = 3) |> cli::cli_alert_warning()
#' #> ! x must have 3 or more elements. # Expected
#'
#' check_length(x, max_len = 1) |> cli::cli_alert_warning()
#' #> ! x must have 1 or less elements. # Expected
#'
#' x <- 1
#' check_length(x, min_len = 2, max_len = 3) |> cli::cli_alert_warning()
#' #> ! x must have a length between 2 and 3. # Expected
check_length <- function( #nolint
    x, #nolint
    len = 1,
    min_len = NULL,
    max_len = NULL,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(len) && is.null(min_len) && is.null(max_len)) {
    names <- collapse_names(
      color = "red",
      names = c("len", "min_len", "max_len"),
      last = "and"
    )

    cli::cli_abort("names cannot all be {.strong NULL}.")
  }

  assert_int(len, lower = 0, null.ok = TRUE)
  assert_int(min_len, lower = 1, null.ok = TRUE)
  assert_int(max_len, lower = 1, null.ok = TRUE)


  if (!is.null(min_len) || !is.null(max_len)) len <- NULL

  if (!is.null(min_len) && !is.null(max_len) && min_len > max_len) {
    cli::cli_abort(
      paste0(
        "{cli::col_red('min_len')} cannot be less than {.strong 'max_len'}."
      )
    )
  }

  names <- collapse_names(.names, color = "red")

  if (isTRUE(null_ok) && is.null(x)) {
    TRUE
  } else if (isFALSE(null_ok) && is.null(x)) {
    glue::glue("{names} cannot be {{.strong NULL}}.")
  } else if (!is.null(len) && !length(x) == len) {
    glue::glue(
      "{names} must have {{.strong {len}}} {{cli::qty({len})}} element{{?s}}."
    )
  } else if ((!is.null(min_len) && !is.null(max_len))) {
    if (length(x) < min_len || length(x) > max_len) {
      glue::glue(
        "{names} must have a length between {{.strong {min_len}}} ",
        "and {{.strong {max_len}}}."
      )
    }
  } else if (!is.null(min_len) && length(x) < min_len) {
    glue::glue("{names} must have {{.strong {min_len}}} or more elements.")
  } else if (!is.null(max_len) && length(x) > max_len) {
    glue::glue("{names} must have {{.strong {max_len}}} or less elements.")
  } else {
    TRUE
  }
}

#' @rdname check_length
#' @export
assert_length <- make_assertion(check_length)

#' @rdname check_length
#' @export
test_length <- make_test(check_length)

# See https://testthat.r-lib.org/articles/custom-expectation.html

# #' @rdname check_length
# #' @export
# expect_length <- function() invisible(NULL)
