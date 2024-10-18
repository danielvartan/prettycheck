test_internet <- function(...) has_internet()

assert_internet <- function() {
  if (isFALSE(has_internet())) {
    cli::cli_abort(paste0(
      "No internet connection was found. ",
      "You must have an internet connection to run this function."
    ))
  } else {
    invisible(TRUE)
  }
}

# expect_internet
