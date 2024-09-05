# Check if 1 or more arguments are not NULL.

test_pick <- function(..., pick = 1) {
  assert_number(pick)

  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  . <- NULL
  # nolint end

  list(...) |>
    lapply(is.null) |>
    unlist() %>%
    which(x = . == FALSE) |>
    length() |>
    magrittr::equals(pick)
}

pick_builder <- function(..., pick, fun) {
  assert_number(pick)
  assert_function(fun)

  test <- test_pick(..., pick = pick)

  if (isFALSE(test)) message_pick(...) |> fun()

  invisible(test)
}

check_pick <- function(..., pick = 1) {
  assert_number(pick)

  pick_builder(..., pick = pick, fun = cli::cli_alert_warning)
}

assert_pick <- function(..., pick = 1) {
  assert_number(pick)

  pick_builder(..., pick = pick, fun = cli::cli_abort)
}

message_pick <- function(..., pick = 1) {
  assert_number(pick)

  names <- collapse_names(
    ...,
    deparsed = TRUE,
    color = "red",
    last = "and",
    names = NULL
  )

  all_null <-
    list(...) |>
    lapply(is.null) |>
    unlist() |>
    all(na.rm = TRUE)

  if (isTRUE(all_null)) {
    glue::glue("{names} cannot both be {{.strong NULL}}.")
  } else if (!test_pick(..., pick = pick)) {
    glue::glue("Only {{.strong {pick}}} of {names} parameters can be assign.")
  } else {
    else_error()
  }
}
