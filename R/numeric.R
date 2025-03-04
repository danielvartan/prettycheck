#' Check if an argument is a [`numeric`][base::numeric] object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_numeric()` check if an argument is a [`numeric`][base::numeric] object.
#'
#' @template param-x
#' @template param-lower_upper
#' @template param-finite
#' @template param-any_missing
#' @template param-all_missing
#' @template param-len
#' @template param-null_ok
#' @template param-.names
#' @template return-default-minus-e
#' @include make_check.R
#' @export
#'
#' @examples
#' test_numeric("a")
#' #> [1] FALSE # Expected
#'
#' test_numeric(lubridate::duration())
#' #> [1] FALSE # Expected
#'
#' test_numeric(1:10)
#' #> [1] TRUE # Expected
#'
#' x <- 1:2
#' check_numeric(x, len = 1) |> cli::cli_alert_warning()
#' #> ! x must have 1 element. # Expected
#'
#' check_numeric(x, min_len = 3) |> cli::cli_alert_warning()
#' #> ! x must have 3 or more elements. # Expected
#'
#' check_numeric(x, max_len = 1) |> cli::cli_alert_warning()
#' #> ! x must have 1 or less elements. # Expected
#'
#' x <- 1
#' check_numeric(1, min_len = 2, max_len = 3) |> cli::cli_alert_warning()
#' #> ! x must have a length between 2 and 3. # Expected
check_numeric <- function( #nolint
    x, #nolint
    lower = -Inf,
    upper = Inf,
    finite = FALSE,
    any_missing = TRUE,
    all_missing = TRUE,
    len = NULL,
    min_len = NULL,
    max_len = NULL,
    # unique = FALSE,
    # sorted = FALSE,
    # names = NULL,
    # typed_missing = FALSE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_number(lower)
  assert_number(upper)
  assert_flag(finite)
  assert_flag(any_missing)
  assert_flag(all_missing)
  assert_int(len, lower = 0, null.ok = TRUE)
  assert_int(min_len, lower = 1, null.ok = TRUE)
  assert_int(max_len, lower = 1, null.ok = TRUE)
  assert_flag(null_ok)
  assert_string(.names)

  names <- collapse_names(.names, color = "red")

  if (isFALSE(null_ok) && is.null(x)) {
    glue::glue("{names} cannot be {{.strong NULL}}.")
  } else if ((!checkmate::test_numeric(x) && !is.null(x)) ||
             lubridate::is.duration(x)) { #nolint
    glue::glue(
      "{names} must be of type {collapse_names('numeric', color = 'blue')}",
      ", not {{.strong {class(x)[1]}}."
    )
  } else if (any(x < lower, na.rm = TRUE)) {
    glue::glue("{names} must be greater than or equal to {{.strong {lower}}}.")
  } else if (any(x > upper, na.rm = TRUE)) {
    glue::glue("{names} must be less than or equal to {{.strong {lower}}}.")
  } else if (isTRUE(finite) && any(abs(x) == Inf, na.rm = TRUE)) {
    glue::glue("{names} must contain only {{.strong finite}} values.")
  } else if (isFALSE(any_missing) && any(is.na(x), na.rm = TRUE)) {
    glue::glue("{names} cannot contain {{.strong any missing}} value.")
  } else if (isFALSE(all_missing) && all(is.na(x), na.rm = TRUE)) {
    glue::glue("{names} cannot have only {{.strong missing}} values.")
  } else if (!is.null(len) && !length(x) == len) {
    glue::glue(
      "{names} must have {{.strong {len}}} {{cli::qty({len})}} element{{?s}}."
    )
  } else if (!is.null(min_len) && length(x) < min_len) {
    glue::glue("{names} must have {{.strong {min_len}}} or more elements.")
  } else if (!is.null(max_len) && length(x) > max_len) {
    glue::glue("{names} must have {{.strong {max_len}}} or less elements.")
  } else {
    TRUE
  }
}

#' @rdname check_numeric
#' @export
assert_numeric <- make_assertion(check_numeric)

#' @rdname check_numeric
#' @export
test_numeric <- make_test(check_numeric)

# See https://testthat.r-lib.org/articles/custom-expectation.html

# #' @rdname check_numeric
# #' @export
# expect_numeric <- checkmate::expect_numeric
