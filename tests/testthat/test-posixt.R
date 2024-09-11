test_that("*_posixt() | general test", {
  expect_true(test_posixt(x = lubridate::as_datetime(1)))
  expect_true(test_posixt(x = as.POSIXlt(lubridate::as_datetime(1))))

  expect_true(test_posixt(
    x = c(lubridate::as_datetime(1), NA), any_missing = TRUE)
  )

  expect_true(test_posixt(x = NULL, null_ok = TRUE))
  expect_false(test_posixt(x = "a"))
  expect_false(test_posixt(x = 1))
  expect_false(test_posixt(x = lubridate::hours()))
  expect_false(test_posixt(x = hms::hms(1)))
  expect_false(test_posixt(x = datasets::iris))

  expect_false(test_posixt(
    x = c(lubridate::as_datetime(1), NA), any_missing = FALSE)
  )

  expect_false(test_posixt(x = NULL, null_ok = FALSE))

  expect_false(test_posixt(
    x = lubridate::as_datetime(1), lower = lubridate::as_datetime(2))
  )

  expect_false(test_posixt(
    x = lubridate::as_datetime(1), upper = lubridate::as_datetime(0))
  )

  checkmate::expect_string(check_posixt(
    x = c(1, NA), any_missing = FALSE
  ),
  "'c\\(1, NA\\)' cannot have missing values"
  )

  checkmate::expect_string(check_posixt(
    x = NULL, null_ok = FALSE
  ),
  "'NULL' cannot be 'NULL'"
  )

  checkmate::expect_string(check_posixt(
    x = lubridate::as_datetime(1), lower = lubridate::as_datetime(2)
  ),
  "Element 1 is not <= "
  )

  checkmate::expect_string(check_posixt(
    x = lubridate::as_datetime(1), upper = lubridate::as_datetime(0)
  ),
  "Element 1 is not >= "
  )

  checkmate::expect_string(check_posixt(
    x = c(1, 1)
  ),
  "Must be of type 'POSIXct' or 'POSIXlt', "
  )

  expect_true(check_posixt(
    x = c(lubridate::as_datetime(1), lubridate::as_datetime(1)))
  )

  expect_true(check_posixt(x = NULL, null_ok = TRUE))

  expect_equal(assert_posixt(
    x = c(lubridate::as_datetime(1), lubridate::as_datetime(1))
  ),
  c(lubridate::as_datetime(1), lubridate::as_datetime(1))
  )

  expect_error(assert_posixt(
    x = c(1, 1)
  ),
  "Assertion on 'c\\(1, 1\\)' failed"
  )
})

test_that("*_posixt() | error test", {
  # checkmate::assert_flag(any_missing)
  expect_error(test_posixt(lubridate::as_datetime(1), any_missing = 1))
  expect_error(check_posixt(lubridate::as_datetime(1), any_missing = 1))

  # checkmate::assert_flag(null_ok)
  expect_error(test_posixt(lubridate::as_datetime(1), null_ok = 1))
  expect_error(check_posixt(lubridate::as_datetime(1), null_ok = 1))
})
