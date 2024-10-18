test_interactive <- function(...) interactive()

# check_interactive

assert_interactive <- function() {
  if (!test_interactive()) {
    stop("You must be in a interactive R session to use this function",
         call. = FALSE)
  }

  invisible(TRUE)
}

# expect_interactive
