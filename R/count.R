# This is temporary!

as_count <- function(x) {
  assert_numeric(x)

  x |>
    as.integer() |>
    abs()
}

test_count <- checkmate::test_count
check_count <- checkmate::check_count
assert_count <- checkmate::assert_count
expect_count <- checkmate::expect_count
