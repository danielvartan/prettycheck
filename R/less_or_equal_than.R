test_less_or_equal_than <- function(x, y, null_ok = FALSE) {
  if (isTRUE(test_first_check_family())) {
    assert_numeric(x)
    assert_numeric(y)
    assert_identical(x, y, type = "length")
    assert_null_ok(x, null_ok)
  }

  all(x <= y)
}
