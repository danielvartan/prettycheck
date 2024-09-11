#' @include make_check.R
NULL

any_builder <- function(x, fun, na_rm = TRUE) {
  assert_function(fun)

  if (is.list(x)) {
    x |>
      lapply(fun) |>
      unlist() |>
      any(na.rm = na_rm)
  } else {
    x |>
      fun() |>
      any(na.rm = na_rm)
  }
}
