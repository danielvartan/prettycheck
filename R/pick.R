#' Check how many arguments were picked
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_pick()` check how many arguments the user picked. It can be used when you
#' want to limit the number of arguments that can be assigned in a function.
#'
#' @param ... Objects to compare.
#' @param pick (optional) an integer number indicating the expected number of
#'   arguments to pick (default: `NULL`) (default: `1`).
#' @param min_pick (optional) an integer number indicating the minimum number of
#'   arguments to pick (default: `NULL`).
#' @param max_pick (optional) an integer number indicating the maximum number of
#'   arguments to pick (default: `NULL`).
#' @param .names (optional) a [character][base::as.character()] vector
#'   containing names for each object in `...` (default:
#'   `prettycheck:::get_names(...)`). This argument is used internally and
#'   should not be set by the user.
#'
#' @template return_a
#' @export
#'
#' @examples
#' x <- 1; y <- NULL
#' test_pick(x, y, pick = 1)
#' #> [1] TRUE # Expected
#'
#' x <- 1; y <- NULL; z <- NULL
#' check_pick(x, y, z, pick = 2) |> cli::cli_alert_warning()
#' #> ! You must pick 2 of the x, y and z arguments. # Expected
#'
#' x <- 1; y <- NULL
#' check_pick(x, y, min_pick = 2) |> cli::cli_alert_warning()
#' #> ! You must assign the x and y arguments. # Expected
#'
#' x <- 1; y <- NULL; z <- NULL
#' check_pick(x, y, z, min_pick = 2) |> cli::cli_alert_warning()
#' #> ! You must pick 2 or more of the x, y and z arguments. # Expected
#'
#' x <- 1; y <- 1; z <- NULL
#' check_pick(x, y, z, max_pick = 1) |> cli::cli_alert_warning()
#' #> ! You must pick 1 or less of the x, y and z arguments. # Expected
check_pick <- function(
    ...,
    pick = NULL,
    min_pick = NULL,
    max_pick = NULL,
    .names = get_names(...)
  ) {
  if (is.null(pick) && is.null(min_pick) && is.null(max_pick)) {
    names <- collapse_names(
      color = "red",
      names = c("pick", "min_pick", "max_pick"),
      last = "and"
    )

    cli::cli_abort(glue::glue("{names} cannot all be {{.strong NULL}}."))
  }

  checkmate::assert_int(pick, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_pick, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_pick, lower = 1, null.ok = TRUE)
  assert_length(list(...), min_len = 2)
  if (!is.null(pick)) assert_length(list(...), min_len = pick)
  if (!is.null(min_pick)) assert_length(list(...), min_len = min_pick)
  checkmate::assert_character(.names)

  names <- collapse_names(color = "red", names = .names, last = "and")

  null_test <-
    list(...) |>
    lapply(is.null) |>
    unlist()

  all_null <- null_test |> all(na.rm = TRUE)
  null_values <- list(...)[null_test]
  not_null_values <- list(...)[!null_test]

  if (isTRUE(all_null)) {
    glue::glue("{names} cannot all be {{.strong NULL}}.")
  } else if (!is.null(pick) && length(.names) == pick ) {
    glue::glue("You must assign the {names} arguments.")
  } else if (!is.null(pick) && length(not_null_values) < pick) {
    glue::glue("You must pick {{.strong {pick}}} of the {names} arguments.")
  } else if (!is.null(pick) && length(not_null_values) > pick) {
    glue::glue("Only {{.strong {pick}}} of {names} arguments can be assign.")
  } else if ((!is.null(min_pick) && !is.null(max_pick)) &&
             (length(not_null_values) < min_pick ||
             length(not_null_values) > max_pick)) {
    glue::glue(
      "You must pick between {{.strong {{cli::col_red({min_pick})}}} ",
      "and {{.strong {{cli::col_red({max_pick})}}} ",
      "of the {names} arguments."
    )
  } else if (!is.null(min_pick) && length(.names) == min_pick) {
    glue::glue("You must assign the {names} arguments.")
  } else if (!is.null(min_pick) && length(not_null_values) < min_pick) {
    glue::glue(
      "You must pick {{.strong {{cli::col_red({min_pick})}}} or more ",
      "of the {names} arguments."
    )
  } else if (!is.null(max_pick) && length(not_null_values) > max_pick) {
    glue::glue(
      "You must pick {{.strong {{cli::col_red({max_pick})}}} or less ",
      "of the {names} arguments."
    )
  } else {
    TRUE
  }
}

#' @rdname check_pick
#' @export
assert_pick <- make_assertion(check_pick)

#' @rdname check_pick
#' @export
test_pick <- make_test(check_pick)

# See <https://testthat.r-lib.org/articles/custom-expectation.html>.

# #' @rdname check_pick
# #' @export
# expect_pick <- function() invisible(NULL)
