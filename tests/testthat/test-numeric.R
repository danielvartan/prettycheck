test_that("*_numeric() | general test", {
  expect_true(test_numeric(x = as.integer(1)))
  expect_true(test_numeric(x = as.double(1)))
  expect_true(test_numeric(x = as.numeric(1)))
  expect_true(test_numeric(x = c(1, NA), any.missing = TRUE))
  expect_true(test_numeric(x = NULL, null_ok = TRUE))
  expect_false(test_numeric(x = lubridate::dhours()))
  expect_false(test_numeric(x = letters))
  expect_false(test_numeric(x = datasets::iris))
  expect_false(test_numeric(x = c(1, NA), any.missing = FALSE))
  expect_false(test_numeric(x = NULL, null_ok = FALSE))
  expect_false(test_numeric(x = 1, lower = 2))
  expect_false(test_numeric(x = 1, upper = 0))

  checkmate::expect_string(check_numeric(
    x = c(1, NA), any.missing = FALSE
  ),
  "'c\\(1, NA\\)' cannot have missing values"
  )

  checkmate::expect_string(check_numeric(
    x = NULL, null_ok = FALSE
  ),
  "'NULL' cannot be 'NULL'"
  )

  checkmate::expect_string(check_numeric(
    x = 1, lower = 2
  ),
  "Element 1 is not <= "
  )

  checkmate::expect_string(check_numeric(
    x = 1, upper = 0
  ),
  "Element 1 is not >= "
  )

  checkmate::expect_string(check_numeric(
    x = c("a", "b")
  ),
  "Must be of type 'numeric', not 'character'"
  )

  expect_true(check_numeric(x = c(1, 1)))
  expect_true(check_numeric(x = NULL, null_ok = TRUE))

  expect_equal(assert_numeric(x = c(1, 1)), c(1, 1))

  expect_error(assert_numeric(
    x = c("a", "b")
  ),
  "Assertion on 'c\\(\"a\", \"b\"\\)' failed"
  )
})
