#' @include make_check.R
NULL

test_data <- function(data, package) {
  assert_string(data)
  assert_string(package)

  assert_namespace(package)
  utils::data(list = data, package = package, envir = environment()) |>
    rutils::shush()

  data %in% ls()
}

# check_data

assert_data <- function(data, package) {
  assert_string(data)
  assert_string(package)

  if (isFALSE(test_data(data, package))) {
    cli::cli_abort(paste0(
      "There's no {cli::col_red(data)} data in ",
      "{cli::col_blue(package)} namespace."
    ))
  } else {
    invisible(TRUE)
  }
}

# expect_data
