test_less_or_equal_than <- function(x, y, null_ok = FALSE) {
  assert_numeric(x)
  assert_numeric(y)
  assert_identical(x, y, type = "length")
  assert_flag(null_ok)

  all(x <= y)
}
