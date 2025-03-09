#' Assert the validity of a `ggplot2` label
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `assert_ggplot_label()` ensures that the input is a valid
#' [`ggplot2`](https://ggplot2.tidyverse.org/) label.
#'
#' A `ggplot2` label can be a [`character`][base::character] string or a
#' [`latexexpression`][latex2exp::TeX] object.
#'
#' @param x A [`character`][base::character] string or a
#'   [`latexexpression`][latex2exp::TeX] object.
#'
#' @return `x` (as [invisible][base::invisible]) if it passes the test; an
#'   error message otherwise.
#'
#' @template param-null_ok
#' @export
#'
#' @examples
#' assert_ggplot_label("Test label")
#' assert_ggplot_label(latex2exp::TeX("$\\overline{x}$"))
assert_ggplot_label <- function(x, null_ok = FALSE) {
  class_options <- c("character", "latexexpression")

  checkmate::assert_flag(null_ok)
  assert_length(x, len = 1, null_ok = null_ok)
  checkmate::assert_multi_class(x, class_options, null.ok = null_ok)

  invisible(x)
}
