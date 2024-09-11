test_that("*_hms() | general test", {
  expect_true(test_hms(x = hms::hms(1)))
  expect_true(test_hms(x = c(hms::hms(1), NA), any_missing = TRUE))
  expect_true(test_hms(x = NULL, null_ok = TRUE))
  expect_false(test_hms(x = "a"))
  expect_false(test_hms(x = 1))
  expect_false(test_hms(x = lubridate::hours()))
  expect_false(test_hms(x = lubridate::dhours()))
  expect_false(test_hms(x = datasets::iris))

  expect_false(test_hms(
    x = c(lubridate::dhours(1), NA), any_missing = FALSE)
  )

  expect_false(test_hms(x = NULL, null_ok = FALSE))

  expect_false(test_hms(
    x = hms::hms(1), lower = hms::hms(2))
  )

  expect_false(test_hms(
    x = hms::hms(1), upper = hms::hms(0))
  )

  checkmate::expect_string(check_hms(
    x = c(1, NA), any_missing = FALSE
  ),
  "'c\\(1, NA\\)' cannot have missing values"
  )

  checkmate::expect_string(check_hms(
    x = NULL, null_ok = FALSE
  ),
  "'NULL' cannot be 'NULL'"
  )

  checkmate::expect_string(check_hms(
    x = hms::hms(1), lower = hms::hms(2)
  ),
  "Element 1 is not <= "
  )

  checkmate::expect_string(check_hms(
    x = hms::hms(1), upper = hms::hms(0)
  ),
  "Element 1 is not >= "
  )

  checkmate::expect_string(check_hms(
    x = c(1, 1)
  ),
  "Must be of type 'hms', not 'numeric'"
  )

  expect_true(check_hms(x = c(hms::hms(1), hms::hms(1))))
  expect_true(check_hms(x = NULL, null_ok = TRUE))

  expect_equal(assert_hms(
    x = c(hms::hms(1), hms::hms(1))
  ),
  c(hms::hms(1), hms::hms(1))
  )

  expect_error(assert_hms(
    x = c(1, 1)
  ),
  "Assertion on 'c\\(1, 1\\)' failed"
  )
})

test_that("*_hms() | error test", {
  # checkmate::assert_flag(any_missing)
  expect_error(test_hms(hms::hms(1), any_missing = 1))
  expect_error(check_hms(hms::hms(1), any_missing = 1))

  # checkmate::assert_flag(null_ok)
  expect_error(test_hms(hms::hms(1), null_ok = 1))
  expect_error(check_hms(hms::hms(1), null_ok = 1))
})
