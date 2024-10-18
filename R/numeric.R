# `*_numeric()` was created as a workaround to deal with cases like
# `is.numeric(lubridate::duration())`. See
# https://github.com/tidyverse/lubridate/issues/942 to learn more.

check_numeric <- function(
    x,
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
  checkmate::assert_string(.names)

  names <- collapse_names(.names, color = "red")

  if (isFALSE(null_ok) && is.null(x)) {
    glue::glue("{names} cannot be {{.strong NULL}}.")
  } else if ((!checkmate::test_numeric(x) && !is.null(x)) ||
             lubridate::is.duration(x)) {
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
      "{names} must have {{.strong {len}}} {{cli::qty(len)}} element{{?s}}."
    )
  } else if (!is.null(min_len) && length(x) < min_len) {
    glue::glue("{names} must have {{.strong {min_len}}} or more elements.")
  } else if (!is.null(max_len) && length(x) > max_len) {
    glue::glue("{names} must have {{.strong {max_len}}} or less elements.")
  } else {
    TRUE
  }
}

assert_numeric <- make_assertion(check_numeric)

test_numeric <- make_test(check_numeric)

# See <https://testthat.r-lib.org/articles/custom-expectation.html>.
expect_numeric <- checkmate::expect_numeric
