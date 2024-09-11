#' Check how many arguments were picked
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_pick()` check how many arguments the user picked. It can be used when you
#' want to limit the number of arguments that can be assigne in a function.
#'
#' @param ... Objects to compare.
#' @param pick (optional) an integer number indicating the expected number of
#'   arguments to pick (default: `NULL`) (default: `1`).
#' @param min_pick (optional) an integer number indicating the minimum number of
#'   arguments to pick (default: `NULL`).
#' @param max_pick (optional) an integer number indicating the maximum number of
#'   arguments to pick (default: `NULL`).
#'
#' @template return_a
#' @include make_check.R
#' @export
#'
#' @examples
#' x <- 1; y <- NULL
#' test_pick(x, y, pick = 1)
#' #> [1] TRUE # Expected
#'
#' x <- 1; y <- NULL; z <- NULL
#' check_pick(x, y, z, pick = 2)
#' #> ! You must pick 2 of the x, y and z arguments. # Expected
#'
#' x <- 1; y <- NULL
#' test_pick(x, y, min_pick = 1)
#' #> [1] TRUE # Expected
#'
#' x <- 1; y <- NULL; z <- NULL
#' check_pick(x, y, z, min_pick = 2)
#' #> ! You must pick 2 or more arguments. # Expected
#'
#' x <- 1; y <- 1; z <- NULL
#' test_pick(x, y, z, max_pick = 1)
#' #> [1] FALSE # Expected
#'
#' x <- 1; y <- 1; z <- NULL
#' check_pick(x, y, max_pick = 1)
#' #> ! You must pick 1 or less arguments. # Expected
test_pick <- function(..., pick = NULL, min_pick = NULL, max_pick = NULL) {
  if (is.null(pick) && is.null(min_pick) && is.null(max_pick)) {
    names <- collapse_names(
      color = "red",
      names = c("pick", "min_pick", "max_pick"),
      last = "and"
    )

    cli::cli_abort(glue::glue("{names} cannot all be {{.strong NULL}}."))
  }

  if (isTRUE(test_first_check_family())) {
    assert_integer_number(pick, lower = 1, null.ok = TRUE)
    assert_integer_number(min_pick, lower = 1, null.ok = TRUE)
    assert_integer_number(max_pick, lower = 1, null.ok = TRUE)
    assert_length(list(...), min_len = 2)

    if (!is.null(pick)) assert_length(list(...), min_len = pick)
    if (!is.null(min_pick)) assert_length(list(...), min_len = min_pick)
  }

  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  . <- NULL
  # nolint end

  not_null <-
    list(...) |>
    lapply(is.null) |>
    unlist() %>%
    which(x = . == FALSE) |>
    length()

  if (not_null == 0) {
    FALSE
  }  else if (!is.null(pick)) {
    not_null == pick
  } else if (!is.null(min_pick)) {
    not_null >= min_pick
  } else if (!is.null(max_pick)) {
    not_null <= max_pick
  } else {
    else_error()
  }
}

message_pick <- function(
    ...,
    pick = 1,
    min_pick = NULL,
    max_pick = NULL,
    names
  ) {
  if (isTRUE(test_first_check_family())) {
    assert_integer_number(pick, null.ok = TRUE)
    assert_integer_number(min_pick, null.ok = TRUE)
    assert_integer_number(max_pick, null.ok = TRUE)
    assert_length(list(...), min_len = 2)
    assert_length(list(...), min_len = pick)
    assert_character(names)
  }

  names <- collapse_names(color = "red", names = names, last = "and")

  null_test <-
    list(...) |>
    lapply(is.null) |>
    unlist()

  all_null <- null_test |> all(na.rm = TRUE)
  null_values <- list(...)[null_test]
  not_null_values <- list(...)[!null_test]

  if (isTRUE(all_null)) {
    glue::glue("{names} cannot all be {{.strong NULL}}.")
  } else if (!is.null(pick) && length(not_null_values) < pick) {
    glue::glue("You must pick {{.strong {pick}}} of the {names} arguments.")
  } else if (!is.null(pick) && length(not_null_values) > pick) {
    glue::glue("Only {{.strong {pick}}} of {names} arguments can be assign.")
  } else if (!is.null(min_pick)) {
    glue::glue(
      "You must pick {{.strong {{cli::col_red(min_pick)}}} or more arguments."
    )
  } else if (!is.null(max_pick)) {
    glue::glue(
      "You must pick {{.strong {{cli::col_red(max_pick)}}} or less arguments."
    )
  } else {
    else_error()
  }
}
#' @rdname test_pick
#' @export
check_pick <- make_check("check", "pick")

#' @rdname test_pick
#' @export
assert_pick <- make_check("assert", "pick")

# expect_pick
