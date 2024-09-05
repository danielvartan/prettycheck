# This is temporary!

test_length <- function(x, len = NULL, min_len = NULL) {
  assert_pick(len, min_len, pick = 1)

  if (!is.null(len)) {
    length(x) == len
  } else if (!is.null(min_len)) {
    length(x) >= min_len
  } else {
    else_error()
  }
}

check_length <- function(x, len = NULL, min_len = NULL) {
  assert_integer_number(len, null.ok = TRUE)
  assert_integer_number(min_len, null.ok = TRUE)
  assert_pick(len, min_len, pick = 1)

  length_builder(
    x, len, min_len, cli::cli_alert_warning, deparse(substitute(x))
  )
}

assert_length <- function(x, len = NULL, min_len = NULL) {
  assert_integer_number(len, null.ok = TRUE)
  assert_integer_number(min_len, null.ok = TRUE)
  assert_pick(len, min_len, pick = 1)

  length_builder(x, len, min_len, cli::cli_abort, deparse(substitute(x)))
}

length_builder <- function(x, len, min_len, fun, name) {
  assert_integer_number(len, null.ok = TRUE)
  assert_integer_number(min_len, null.ok = TRUE)
  assert_pick(len, min_len, pick = 1)
  assert_function(fun)
  assert_string(name)

  test <- test_length(x, len = len, min_len = min_len)

  if (isFALSE(test)) message_length(x, len, min_len, name) |> fun()

  invisible(test)
}

message_length <- function(x, len = NULL, min_len = NULL, name) {
  assert_integer_number(len, null.ok = TRUE)
  assert_integer_number(min_len, null.ok = TRUE)
  assert_pick(len, min_len, pick = 1)
  assert_string(name)

  name <- collapse_names(color = "red", names = name)

  if (!is.null(len)) {
    glue::glue("{name} must have {{.strong {len}}} elements.")
  } else if (!is.null(min_len)) {
    glue::glue("{name} must have {{.strong {min_len}}} or more elements.")
  } else {
    else_error()
  }
}
