#' Check for internet connectivity
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_internet()` check if there is an active internet connection.
#'
#' @template return-default-minus-e
#' @export
#'
#' @examples
#' if (interactive()) {
#'   test_internet()
#' }
check_internet <- function() {
  if (!test_internet()) {
    paste0(
      "No internet connection was found. ",
      "You must have an internet connection to run this function."
    )
  } else {
    TRUE
  }
}

#' @rdname check_internet
#' @export
test_internet <- function() has_internet()

#' @rdname check_internet
#' @export
assert_internet <- function() {
  if (!test_internet()) {
    cli::cli_abort(check_internet())
  } else {
    invisible(TRUE)
  }
}

# expect_internet
