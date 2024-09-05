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
#' @param type A string corresponding to the type of comparison to perform.
#'   Valid values: `"value"`, `"length"`, and `"class"` (default: `"value"`).
#'
#' @template return_a
#' @export
#'
#' @examples
#' x <- 1; y <- 1
#' test_identical(x, y, type = "value")
#' #> TRUE # Expected
#'
#' x <- 1; y <- 2
#' check_identical(x, y, type = "value")
#' #> ! x and y must have identical values. # Expected
#'
#' x <- letters; y <- LETTERS
#' test_identical(x, y, type = "length")
#' #> TRUE # Expected
#'
#' x <- letters; y <- 1:2
#' check_identical(x, y, type = "length")
#' #> ! x and y must have identical lengths. # Expected
#'
#' x <- "a"; y <- c("a", "b")
#' test_identical(x, y, type = "class")
#' #> TRUE # Expected
#'
#' x <- "a"; y <- 1
#' check_identical(x, y, type = "class")
#' #> ! x and y must have identical classes. # Expected
test_identical <- function(..., type = "value") {
  assert_length(list(...), min_len = 2)
  assert_choice(type, c("value", "length", "class"))

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

#' @rdname test_identical
#' @export
check_identical <- function(..., type = "value") {
  assert_length(list(...), min_len = 2)
  assert_choice(type, c("value", "length", "class"))

  identical_builder(..., type = type, fun = cli::cli_alert_warning)
}

#' @rdname test_identical
#' @export
assert_identical <- function(..., type = "value") {
  assert_length(list(...), min_len = 2)
  assert_choice(type, c("value", "length", "class"))

  identical_builder(..., type = type, fun = cli::cli_abort)
}

#' @rdname test_identical
#' @export
expect_identical <- function(..., type = "value") {
  assert_length(list(...), min_len = 2)
  assert_choice(type, c("value", "length", "class"))

  identical_builder(..., type = type, fun = cli::cli_abort, expected = TRUE)
}

identical_builder <- function(..., type, fun, expected = FALSE) {
  assert_length(list(...), min_len = 2)
  assert_choice(type, c("value", "length", "class"))
  assert_function(fun)
  assert_flag(expected)

  test <- test_identical(..., type = type)

  if (type == "value" && isFALSE(test)) {
    message_identical(..., type = "value", expected = expected) |> fun()
  } else if (type == "length" && isFALSE(test)) {
    message_identical(..., type = "length", expected = expected) |> fun()
  } else if (type == "class" && isFALSE(test)) {
    message_identical(..., type = "class", expected = expected) |> fun()
  } else {
    invisible(test)
  }
}

message_identical <- function(..., type = "value", expected = FALSE) {
  assert_length(list(...), min_len = 2)
  assert_choice(type, c("value", "length", "class"))
  assert_flag(expected)

  names <- collapse_names(..., deparsed = TRUE, color = "red", last = "and")

  if (type == "value" && isFALSE(expected)) {
    glue::glue("{names} must have identical values.")
  } else if (type == "length" && isFALSE(expected)) {
    glue::glue("{names} must have identical lengths.")
  } else if (type == "class" && isFALSE(expected)) {
    glue::glue("{names} must belong to the same class.")
  } else if (type == "value" && isTRUE(expected)) {
    glue::glue("{names} are expected to have identical values.")
  } else if (type == "length" && isTRUE(expected)) {
    glue::glue("{names} are expected to have identical lengths.")
  } else if (type == "class" && isTRUE(expected)) {
    glue::glue("{names} are expected to belong to the same class.")
  } else {
    else_error()
  }
}
