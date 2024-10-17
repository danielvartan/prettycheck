#' @include make_check.R
NULL

any_infinite <- function(x, na_rm = TRUE) any_builder(x, is.infinite, na_rm)

# test_infinite
# check_infinite
# assert_infinite
