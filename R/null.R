#' @include make_check.R
NULL

# This is temporary!

any_null <- function(x, na_rm = TRUE) any_builder(x, is.null, na_rm)

test_null <- checkmate::test_null
check_null <- checkmate::check_null
assert_null <- checkmate::assert_null

expect_null <- checkmate::makeExpectationFunction(
  checkmate::check_null,
  use.namespace = TRUE
)

# names <-
#   match.call() |>
#   magrittr::inset2(as.name("test_identical")) |>
#   eval.parent() |>
#   invisible()

message_null <- function(name) {
  assert_character(name)

  name <- collapse_names(color = "red", names = name)

  glue::glue("{name} cannot be {{.strong NULL}}.")
}
