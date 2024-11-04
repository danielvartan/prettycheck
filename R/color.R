assert_color <- function(color, null_ok = FALSE, na_ok = FALSE) {
  color_pattern <- "(?i)^#[a-f0-9]{3}$|^#[a-f0-9]{6}$|^transparent$"

  checkmate::assert_flag(null_ok)
  checkmate::assert_flag(na_ok)

  name <- deparse(substitute(color))

  if (is.null(color) && isFALSE(null_ok)) {
    cli::cli_abort(
      paste0(
        "{.strong {cli::col_red(name)}} cannot be {.strong NULL}."
      )
    )
  }

  if (!is.null(color)) {
    if (is.na(color) && isFALSE(na_ok)) {
      cli::cli_abort(
        paste0(
          "{.strong {cli::col_red(name)}} cannot be {.strong NA}."
        )
      )
    }
  }

  if (!is.null(color) && !is.na(color) &&
      !color %in% grDevices::colors() &&
      !checkmate::test_string(color, pattern = color_pattern)) {
    cli::cli_abort(
      paste0(
        "{.strong {cli::col_red(name)}} is not a valid color code. ",
        "It must contain a hexadecimal color code or one of the ",
        "values in {.strong {cli::col_blue('grDevices::color()')}}."
      )
    )
  }

  invisible(NULL)
}
