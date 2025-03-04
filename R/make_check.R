make_test <- function(check_fun) {
  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  .names <- NULL
  # nolint end

  type <- extract_check_type(deparse(substitute(check_fun)))

  checkmate::assert_function(check_fun)
  checkmate::assert_set_equal(type, "check")

  out <- function() {
    if ("..." %in% names(formals(check_fun))) {
      # `.names` must be provided, since it is a dot-argument.
      check <- do.call( #nolint
        check_fun,
        c(list(...), env_get_list(), list(.names = .names))
      )
    } else {
      # `.names` must be provided, since it is a dot-argument.
      check <- do.call(check_fun, c(env_get_list(), list(.names = .names)))
    }

    if (isTRUE(check)) TRUE else FALSE
  }

  formals(out) <- formals(check_fun)

  out
}

make_assertion <- function(check_fun) {
  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  .names <- x <- NULL
  # nolint end

  type <- extract_check_type(deparse(substitute(check_fun)))

  checkmate::assert_function(check_fun)
  checkmate::assert_set_equal(type, "check")

  out <- function() {
    if ("..." %in% names(formals(check_fun))) {
      # `.names` must be provided, since it is a dot-argument.
      check <- do.call( #nolint
        check_fun,
        c(list(...), env_get_list(), list(.names = .names))
      )

      if (isTRUE(check)) invisible(list(...)) else cli::cli_abort(check) #nolint
    } else {
      # `.names` must be provided, since it is a dot-argument.
      check <- do.call(check_fun, c(env_get_list(), list(.names = .names)))

      if (isTRUE(check)) invisible(x) else cli::cli_abort(check)
    }
  }

  formals(out) <- formals(check_fun)

  out
}

# Don't move!
extract_check_type <- function(check_name) {
  check_name |>
    stringr::str_extract("^(test|check|assert|expect)*(?=_)")
}

# Don't move!
extract_check_family <- function(check_name) {
  check_name |>
    stringr::str_extract(
      "(?<=test_).*|(?<=check_).*|(?<=assert_).*|(?<=expect_).*"
    )
}
