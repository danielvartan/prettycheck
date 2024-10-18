test_null_ok <- function(x, null_ok = FALSE) {
  assert_flag(null_ok)

  if (any_null(x) && isFALSE(null_ok)) {
    FALSE
  } else {
    TRUE
  }
}

null_ok_builder <- function(x, null_ok, fun, names) {
  assert_flag(null_ok)
  assert_function(fun)
  assert_character(names)

  test <- test_null_ok(x, null_ok)

  if (isFALSE(test)) message_null(names) |> fun()

  invisible(test)
}

check_null_ok <- function(x, null_ok = FALSE) {
  assert_flag(null_ok)

  null_ok_builder(x, null_ok, cli::cli_alert_warning, deparse(substitute(x)))
}

assert_null_ok <- function(x, null_ok = FALSE) {
  assert_flag(null_ok)

  null_ok_builder(x, null_ok, cli::cli_abort, deparse(substitute(x)))
}
