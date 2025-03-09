#' Assert a color input
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `assert_color()` ensures that the provided color values are valid color
#' names recognized by `grDevices::colors()` or valid hexadecimal color codes
#' (`#RGB` or `#RRGGBB`).
#'
#' @param color A [`character`][base::character] vector with color names or a
#'   hexadecimal color codes.
#'
#' @return `color` (as [invisible][base::invisible]) if it passes the test; an
#'   error message otherwise.
#'
#' @template param-any_missing
#' @template param-null_ok
#' @export
#'
#' @examples
#' assert_color("#FFF")
#' assert_color("#FF5733")
#' assert_color("#FF0000FF")
#' assert_color("blue")
#' assert_color("transparent")
#' assert_color(c("#FF5733", "blue", "transparent"))
assert_color <- function(
    color, #nolint
    any_missing = FALSE,
    null_ok = FALSE
  ) {
  assert_flag(any_missing)
  assert_flag(null_ok)
  assert_character(color, any.missing = any_missing, null.ok = null_ok)

  color_pattern <-
    paste0(
      c(
        "(?i)^#[a-f0-9]{3}$",
        "(?i)^#[a-f0-9]{6}$",
        "(?i)^#[a-f0-9]{8}$",
        "(?i)^transparent$"
      ),
      collapse = "|"
    )

  for (i in color) {
    if (!i %in% grDevices::colors() && #nolint
        !test_string(i, pattern = color_pattern)) { #nolint
      cli::cli_abort(
        paste0(
          "{.strong {cli::col_red(i)}} is not a valid color code. ",
          "It must contain a hexadecimal color code or one of the ",
          "values in {.strong {cli::col_blue('grDevices::color()')}}."
        )
      )
    }
  }

  invisible(color)
}

#' Assert special color inputs
#'
#' `r lifecycle::badge("experimental")`
#'
#' `assert_color_options()` ensures that the provided color options are valid.
#' It checks if the `color_low` and `color_high` arguments are valid colors
#' and are provided together, or, alternatively, ensures that `viridis` color
#' palette names are valid. These options are mutually exclusive.
#'
#' @param color_low,color_high (Optional) A [`character`][base::character]
#'   vector representing color names or hexadecimal color codes
#'   (Default: `NULL`).
#' @param viridis (Optional) A [`character`][base::character] vector
#'   representing viridis color palette names (Default: `NULL`).
#'
#' @return An [invisible][base::invisible] `TRUE` if the input is valid;
#'   an error message otherwise.
#'
#' @export
#'
#' @examples
#' assert_color_options(color_low = "#FF5733", color_high = "#33FF57")
#'
#' assert_color_options(
#'   color_low = c("#FF5733", "#FFF"),
#'   color_high = c("#33FF57", "#000")
#' )
#'
#' assert_color_options(viridis = "magma")
#' assert_color_options(viridis = c("magma", "viridis"))
assert_color_options <- function( #nolint
    color_low = NULL, # nolint
    color_high = NULL,
    viridis = NULL
  ) {
  assert_pick(color_low, color_high, viridis, min_pick = 1)
  assert_character(color_low, null.ok = TRUE)
  assert_character(color_high, null.ok = TRUE)

  if (is.null(color_low) && !is.null(color_high) ||
      !is.null(color_low) && is.null(color_high)) { #nolint
    cli::cli_abort(
      paste0(
        "You must provide both ",
        "{.strong {cli::col_blue('color_low')}} and ",
        "{.strong {cli::col_red('color_high')}} ",
        "arguments at the same time."
      )
    )
  } else if ((!is.null(color_low) || !is.null(color_high)) &&
             !is.null(viridis)) { #nolint
    cli::cli_abort(
      paste0(
        "You can't use both ",
        "{.strong {cli::col_blue('color_low/color_high')}} and ",
        "{.strong {cli::col_red('viridis')}} ",
        "arguments at the same time."
      )
    )
  }

  assert_color(color_low, null_ok = TRUE)
  assert_color(color_high, null_ok = TRUE)

  viridis_choices <- c(
    "magma", "A", "inferno", "B", "plasma", "C", "viridis", "D",
    "cividis", "E", "rocket", "F", "mako", "G", "turbo", "H"
  )

  for (i in viridis) {
    if (!i %in% viridis_choices) {
      cli::cli_abort(
        paste0(
          "{.strong {cli::col_red(i)}} is not a valid viridis color palette. ",
          "It must be one of the following values:\n\n",
          glue::glue_collapse(
            paste0("{.strong ", viridis_choices, "}"),
            sep = ", ",
            last = ", or "
          )
        )
      )
    }
  }

  invisible(TRUE)
}
