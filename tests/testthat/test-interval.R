test_that("*_interval() | general test", {
  int <- lubridate::interval(
    as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-02 00:00:00"))
  int_lower <- lubridate::interval(
    as.POSIXct("2020-01-01 01:00:00"), as.POSIXct("2020-01-02 00:00:00"))
  int_upper <- lubridate::interval(
    as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-02 01:00:00"))

  expect_true(test_interval(int))
  expect_true(test_interval(c(int, lubridate::as.interval(NA)),
                            any_missing = TRUE))
  expect_true(test_interval(NULL, null_ok = TRUE))
  expect_true(test_interval(int, lower = int_lower))
  expect_true(test_interval(int, upper = int_upper))
  expect_false(test_interval("a"))
  expect_false(test_interval(1))
  expect_false(test_interval(lubridate::hours()))
  expect_false(test_interval(hms::hms(1)))
  expect_false(test_interval(datasets::iris))
  expect_false(test_interval(c(int, NA), any_missing = FALSE))
  expect_false(test_interval(NULL, null_ok = FALSE))
  expect_false(test_interval(int, lower = int_upper))
  expect_false(test_interval(int, upper = int_lower))

  checkmate::expect_string(
    check_interval(c(1, NA), any_missing = FALSE),
    pattern = "'c\\(1, NA\\)' cannot have missing values"
  )
  checkmate::expect_string(check_interval(NULL, null_ok = FALSE),
                           pattern = "'NULL' cannot be 'NULL'")
  checkmate::expect_string(check_interval(int, lower = int_upper),
                           pattern = "Element 1 is not >= ")
  checkmate::expect_string(check_interval(int, upper = int_lower),
                           pattern = "Element 1 is not <= ")
  checkmate::expect_string(check_interval(c(1, 1)),
                           pattern = "Must be of type 'Interval'")
  expect_true(check_interval(c(int, int_lower)))
  expect_true(check_interval(NULL, null_ok = TRUE))

  expect_equal(assert_interval(c(int, int_lower)), c(int, int_lower))
  expect_error(assert_interval(c(1, 1)), "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_interval() | error test", {
  # checkmate::assert_flag(any_missing)
  expect_error(test_interval(lubridate::as_datetime(1), any_missing = 1))
  expect_error(check_interval(lubridate::as_datetime(1), any_missing = 1))

  # checkmate::assert_flag(null_ok)
  expect_error(test_interval(lubridate::as_datetime(1), null_ok = 1))
  expect_error(check_interval(lubridate::as_datetime(1), null_ok = 1))
})
