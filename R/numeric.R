# `*_numeric()` was created as a workaround to deal with cases like
# `is.numeric(lubridate::duration())`. See
# https://github.com/tidyverse/lubridate/issues/942 to learn more.

formals_numeric <- alist(
  x = ,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  any_missing = TRUE,
  all_missing = TRUE,
  len = NULL,
  min_len = NULL,
  max_len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed_missing = FALSE,
  null_ok = FALSE
)

test_numeric <- function() {
  test <- checkmate::test_numeric(
    x,
    lower = lower,
    upper = upper,
    finite = finite,
    any.missing = any_missing,
    all.missing = all_missing,
    len = len,
    min.len = min_len,
    max.len = max_len,
    unique = unique,
    sorted = sorted,
    names = names,
    typed.missing = typed_missing,
    null.ok = null_ok
  )

  if (lubridate::is.duration(x)) {
    FALSE
  } else {
    test
  }
}

formals(test_numeric) <- formals_numeric

message_numeric <- function() {
  .name <- collapse_names(.name, color = "red")

  if (!checkmate::test_numeric(x)) {
    glue::glue(
      "{.name} must be of type {collapse_names('numeric', color = 'blue')}",
      ", not {{.strong {class(x)[1]}}."
    )
  } else if (!is.null(len) && !length(x) == len) {
    glue::glue(
      "{.name} must have {{.strong {len}}} {{cli::qty(len)}} element{{?s}}."
    )
  } else if (!is.null(min_len) && length(x) < min_len) {
    glue::glue("{.name} must have {{.strong {min_len}}} or more elements.")
  } else if (!is.null(max_len) && length(x) > max_len) {
    glue::glue("{.name} must have {{.strong {max_len}}} or less elements.")
  } else {
    glue::glue(
      "{.name} does not meet the function requirements."
    )
  }
}

formals(message_numeric) <- c(formals_numeric, alist(.name = NULL))

constructor_numeric <- function() {
  checkmate::assert_choice(.fun_type, c("check", "assert", "expect"))

  obj_list <- env_get_list()
  test <- do.call(test_numeric, obj_list)

  message <- do.call(
    message_numeric,
    c(obj_list, list(.name = deparse(substitute(x))))
  )

  cli_fun <- ifelse(
    .fun_type == "check",
    cli::cli_alert_warning,
    cli::cli_abort
  )

  if (isFALSE(test)) {
    message |> cli_fun()

    invisible(FALSE)
  } else {
    invisible(x)
  }
}

formals(constructor_numeric) <- c(
  formals_numeric, alist(.names = NULL, .fun_type = NULL)
)

check_numeric <- function() {
  do.call(
    constructor_numeric,
    c(
      env_get_list(),
      list(.name = deparse(substitute(x)), .fun_type = "check")
    )
  )
}

formals(check_numeric) <- formals_numeric

assert_numeric <- function() {
  do.call(
    constructor_numeric,
    c(
      env_get_list(),
      list(.name = deparse(substitute(x)), .fun_type = "assert")
    )
  )
}

formals(assert_numeric) <- formals_numeric

# See <https://testthat.r-lib.org/articles/custom-expectation.html>.
expect_numeric <- checkmate::expect_numeric
