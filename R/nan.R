#' @include make_check.R
NULL

any_nan <- function(x, na_rm = TRUE) any_builder(x, is.nan, na_rm)

# test_nan
# check_nan
# assert_nan
# expect_nan
