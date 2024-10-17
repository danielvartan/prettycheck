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
#' check_identical(x, y, type = "value")
#' #> ! x and y must have identical values. # Expected
#'
#' x <- letters; y <- LETTERS
#' test_identical(x, y, type = "length")
#' #> [1] TRUE # Expected
#'
#' x <- letters; y <- 1:2
#' check_identical(x, y, type = "length")
#' #> ! x and y must have identical lengths. # Expected
#'
#' x <- "a"; y <- c("a", "b")
#' test_identical(x, y, type = "class")
#' #> [1] TRUE # Expected
#'
#' x <- "a"; y <- 1
#' check_identical(x, y, type = "class")
#' #> ! x and y must belong to the same class. # Expected
test_identical <- function(..., type = "value") {
  if (isTRUE(test_first_check_family())) {
    assert_length(list(...), min_len = 2)
    assert_choice(type, c("value", "length", "class"))
  }

  values <- list(...)

  if (type == "value") {
    values |>
      unique() |>
      length() |>
      magrittr::equals(1)
  } else if (type == "length") {
    values |>
      vapply(length, integer(1)) |>
      unique() |>
      length() |>
      magrittr::equals(1)
  } else if (type == "class") {
    values |>
      lapply(class) |>
      unique() |>
      length() |>
      magrittr::equals(1)
  } else {
    else_error()
  }
}

message_identical <- function(..., type = "value", names) {
  if (isTRUE(test_first_check_family())) {
    assert_length(list(...), min_len = 2)
    assert_choice(type, c("value", "length", "class"))
    assert_character(names)
  }

  names <- collapse_names(color = "red", names = names, last = "and")

  if (type == "value") {
    glue::glue("{names} must have identical values.")
  } else if (type == "length") {
    glue::glue("{names} must have identical lengths.")
  } else if (type == "class") {
    glue::glue("{names} must belong to the same class.")
  } else {
    else_error()
  }
}

#' @rdname test_identical
#' @export
check_identical <- make_check("check", "identical")

#' @rdname test_identical
#' @export
assert_identical <- make_check("assert", "identical")

#' @rdname test_identical
#' @export
expect_identical <- make_check("expect", "identical")
