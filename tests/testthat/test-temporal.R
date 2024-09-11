test_that("*_temporal() | general test", {
  expect_true(test_temporal(lubridate::dhours()))
  expect_true(test_temporal(lubridate::hours()))
  expect_true(test_temporal(as.difftime(1, units = "secs")))
  expect_true(test_temporal(hms::hms(1)))
  expect_true(test_temporal(as.Date("2000-01-01")))
  expect_true(test_temporal(lubridate::as_datetime(1)))
  expect_true(test_temporal(as.POSIXlt(lubridate::as_datetime(1))))
  expect_true(test_temporal(lubridate::as.interval(
    lubridate::dhours(), lubridate::as_datetime(0))))
  expect_true(test_temporal(NULL, null_ok = TRUE))
  expect_false(test_temporal(1))
  expect_false(test_temporal(letters))
  expect_false(test_temporal(datasets::iris))
  expect_false(test_temporal(lubridate::dhours(), rm = "Duration"))
  expect_false(test_temporal(lubridate::hours(), rm = "Period"))
  expect_false(test_temporal(as.difftime(1, units = "secs"), rm = "difftime"))
  expect_false(test_temporal(hms::hms(1), rm = "hms"))
  expect_false(test_temporal(as.Date("2000-01-01"), rm = "Date"))
  expect_false(test_temporal(lubridate::as_datetime(1), rm = "POSIXct"))
  expect_false(test_temporal(as.POSIXlt(lubridate::as_datetime(1)),
                             rm = "POSIXlt"))
  expect_false(test_temporal(lubridate::as.interval(
    lubridate::dhours(), lubridate::as_datetime(0)), rm = "Interval"))
  expect_false(test_temporal(c(1, NA), any_missing = FALSE))
  expect_false(test_temporal(NULL, null_ok = FALSE))

  checkmate::expect_string(check_temporal(c(1, 1)),
                           pattern = "Must be a temporal object ")
  checkmate::expect_string(check_temporal(c(1, NA), any_missing = FALSE),
                           pattern = "'c\\(1, NA\\)' cannot have missing ")
  checkmate::expect_string(check_temporal(NULL, null_ok = FALSE),
                           pattern = "'NULL' cannot be 'NULL'")
  expect_true(check_temporal(c(lubridate::hours(1), lubridate::hours(1))))
  expect_true(check_temporal(NULL, null_ok = TRUE))

  expect_equal(assert_temporal(c(lubridate::hours(1), lubridate::hours(1))),
               c(lubridate::hours(1), lubridate::hours(1)))
  expect_error(assert_temporal(c(1, 1)), "Assertion on 'c\\(1, 1\\)' failed")
})
