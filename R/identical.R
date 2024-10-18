#' Check if multiple objects are identical
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_identical()` check if multiple objects are identical by value, length, or
# class.
#'
#' @param ... Objects to compare.
#' @param type (optional) a string corresponding to the type of comparison to
#'   perform. Valid values: `"value"`, `"length"`, and `"class"` (default:
#'   `"value"`).
#' @param .names (optional) a [character][base::as.character()] vector
#'   containing names for each object in `...` (default:
#'   `prettycheck:::get_names(...)`).  This argument is used internally and
#'   should not be set by the user.
#'
#' @template return_a
#' @include make_check.R
#' @export
#'
#' @examples
#' x <- 1; y <- 1
#' test_identical(x, y, type = "value")
#' #> [1] TRUE # Expected
#'
#' x <- 1; y <- 2
#' check_identical(x, y, type = "value") |> cli::cli_alert_warning()
#' #> ! x and y must have identical values. # Expected
#'
#' x <- letters; y <- 1:2
#' check_identical(x, y, type = "length") |> cli::cli_alert_warning()
#' #> ! x and y must have identical lengths. # Expected
#'
#' x <- "a"; y <- 1
#' check_identical(x, y, type = "class") |> cli::cli_alert_warning()
#' #> ! x and y must belong to the same class. # Expected
check_identical <- function(
    ...,
    type = "value",
    .names = get_names(...)
  ) {
  assert_length(list(...), min_len = 2)
  assert_choice(type, c("value", "length", "class"))
  checkmate::assert_character(.names)

  values <- list(...)
  names <- collapse_names(color = "red", names = .names, last = "and")

  if (type == "value") {
    test <-
      values |>
      unique() |>
      length() |>
      magrittr::equals(1)

    if (isFALSE(test)) {
      glue::glue("{names} must have identical values.")
    } else {
      TRUE
    }
  } else if (type == "length") {
    test <-
      values |>
      vapply(length, integer(1)) |>
      unique() |>
      length() |>
      magrittr::equals(1)

    if (isFALSE(test)) {
      glue::glue("{names} must have identical lengths.")
    } else {
      TRUE
    }
  } else if (type == "class") {
    test <-
      values |>
      lapply(class) |>
      unique() |>
      length() |>
      magrittr::equals(1)

    if (isFALSE(test)) {
      glue::glue("{names} must belong to the same class.")
    } else {
      TRUE
    }
  } else {
    else_error()
  }
}

#' @rdname check_identical
#' @export
assert_identical <- make_assertion(check_identical)

#' @rdname check_identical
#' @export
test_identical <- make_test(check_identical)

# See <https://testthat.r-lib.org/articles/custom-expectation.html>.

# #' @rdname test_identical
# #' @export
# expect_identical <- function() invisible(NULL)
