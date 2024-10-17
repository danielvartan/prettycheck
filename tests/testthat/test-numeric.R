test_that("*_numeric() | general test", {
  expect_true(test_numeric(x = as.integer(1)))
  expect_true(test_numeric(x = as.double(1)))
  expect_true(test_numeric(x = as.numeric(1)))
  expect_true(test_numeric(x = c(1, NA), any_missing = TRUE))
  expect_true(test_numeric(x = NULL, null_ok = TRUE))
  expect_false(test_numeric(x = lubridate::dhours()))
  expect_false(test_numeric(x = letters))
  expect_false(test_numeric(x = datasets::iris))
  expect_false(test_numeric(x = c(1, NA), any_missing = FALSE))
  expect_false(test_numeric(x = NULL, null_ok = FALSE))
  expect_false(test_numeric(x = 1, lower = 2))
  expect_false(test_numeric(x = 1, upper = 0))

  check_numeric(x = c(1, NA), any_missing = FALSE) |>
    testthat::expect_message()

  check_numeric(x = NULL, null_ok = FALSE) |>
    testthat::expect_message()

  check_numeric(x = 1, lower = 2) |>
    testthat::expect_message()

  check_numeric(x = 1, upper = 0) |>
    testthat::expect_message()

  check_numeric(x = c("a", "b")) |>
    testthat::expect_message()

  expect_equal(assert_numeric(x = c(1, 1)), c(1, 1))

  assert_numeric( x = c("a", "b")) |>
    testthat::expect_error()
})
