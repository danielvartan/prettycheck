#' Check if R is being used interactively
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_interactive()` check if the R session is interactive.
#'
#' @template return-default-minus-e
#' @export
#'
#' @examples
#' if (interactive()) {
#'   test_interactive()
#'   #> [1] TRUE # Expected
#' }
check_interactive <- function() {
  if (!test_interactive()) {
    paste0(
      "You must be in an {.strong interactive} R session to use this ",
      "function."
    )
  } else {
    TRUE
  }
}

#' @rdname check_interactive
#' @export
test_interactive <- function() interactive()

#' @rdname check_interactive
#' @export
assert_interactive <- function() {
  if (!test_interactive()) {
    cli::cli_abort(check_interactive())
  } else {
    invisible(TRUE)
  }
}

# expect_interactive
