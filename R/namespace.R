test_namespace <- function(x) {
  assert_string(x)
  requireNamespace(x, quietly = TRUE)
}

check_namespace <- function(x, null_ok = FALSE, name = deparse(substitute(x))) {
  assert_string(x, null.ok = TRUE)
  assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(name), " cannot have 'NULL' values")
  } else if (isFALSE(test_namespace(x))) {
    paste0("There's no namespace called ",  glue::single_quote(x))
  } else {
    TRUE
  }
}

assert_namespace <- checkmate::makeAssertionFunction(check_namespace)

# expect_namespace
