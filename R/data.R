#' Check if a dataset exists in a package
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
# `*_data()` check if a dataset is present in a specific package.
#'
#' @param data A [`character`][base::character] string of the dataset name.
#' @param package A [`character`][base::character] string of the package name.
#'
#' @template return-default-true-minus-ce
#' @export
#'
#' @examples
#' if (requireNamespace("datasets", quietly = TRUE)) {
#'   test_data("mtcars", "datasets")
#'   #> [1] TRUE # Expected
#' }
check_data <- function(data, package) {
  if (!test_data(data, package)) {
    paste0(
      "There is no {cli::col_red(data)} dataset in the ",
      "{cli::col_blue(package)} package namespace."
    )
  } else {
    TRUE
  }
}

#' @rdname check_data
#' @export
test_data <- function(data, package) {
  assert_string(data)
  assert_string(package)

  assert_namespace(package)
  utils::data(list = data, package = package, envir = environment()) |>
    shush()

  data %in% ls()
}

#' @rdname check_data
#' @export
assert_data <- function(data, package) {
  if (!test_data(data, package)) {
    cli::cli_abort(check_data(data, package))
  } else {
    invisible(TRUE)
  }
}

# expect_data
