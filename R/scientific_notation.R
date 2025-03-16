#' Check if a `numeric` vector is in scientific notation
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_scientific_notation()` check if an argument is formatted in scientific
#' notation.
#'
#' @param x (Optional) A [`numeric`][base::numeric] vector.
#'
#' @template param-any_missing
#' @template param-null_ok
#' @template param-.names
#' @template param-checkmate-minus-.var_name
#' @template return-default
#' @export
#'
#' @examples
#' opt <- getOption("scipen")
#'
#' options(scipen = 0)
#' test_scientific_notation(0.00000000000000000000000001)
#' #> [1] TRUE # Expected
#'
#' options(scipen = 999)
#' test_scientific_notation(0.00000000000000000000000001)
#' #> [1] FALSE # Expected
#'
#' options(scipen = opt)
check_scientific_notation <- function(
    x, #nolint
    any_missing = TRUE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_numeric(x, any_missing = TRUE, null_ok = TRUE)
  assert_flag(any_missing)
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    paste0(glue::single_quote(.names), " cannot have missing values")
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(.names), " cannot be 'NULL'")
  } else if (!test_scientific_notation(x)) {
    paste0(
      glue::single_quote(.names),
      " is not formatted in scientific notation."
    )
  } else {
    TRUE
  }
}

#' @rdname check_scientific_notation
#' @export
test_scientific_notation <- function(
    x, #nolint
    any_missing = TRUE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_numeric(x, any_missing = TRUE, null_ok = TRUE)
  assert_flag(any_missing)
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    FALSE
  } else {
    x |>
      format() |>
      grepl("e", x = _) |>
      any(na.rm = TRUE)
  }
}

#' @rdname check_scientific_notation
#' @export
assert_scientific_notation <- checkmate::makeAssertionFunction(
  check_scientific_notation
)

#' @rdname check_scientific_notation
#' @export
expect_scientific_notation <- checkmate::makeExpectationFunction(
  check_scientific_notation,
  use.namespace = TRUE
)
