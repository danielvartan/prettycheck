test_that("*_duration() | general test", {
  expect_true(test_duration(x = lubridate::dhours(1)))

  expect_true(test_duration(
    x = c(lubridate::dhours(1), NA), any.missing = TRUE)
  )

  expect_true(test_duration(x = NULL, null_ok = TRUE))
  expect_false(test_duration(x = "a"))
  expect_false(test_duration(x = 1))
  expect_false(test_duration(x = lubridate::hours()))
  expect_false(test_duration(x = hms::hms(1)))
  expect_false(test_duration(x = datasets::iris))

  expect_false(test_duration(
    x = c(lubridate::dhours(1), NA), any.missing = FALSE)
  )

  expect_false(test_duration(x = NULL, null_ok = FALSE))

  expect_false(test_duration(
    x = lubridate::dhours(1), lower = lubridate::dhours(2))
  )

  expect_false(test_duration(
    x = lubridate::dhours(1), upper = lubridate::dhours(0))
  )

  checkmate::expect_string(check_duration(
    x = c(1, NA), any.missing = FALSE
  ),
  "'c\\(1, NA\\)' cannot have missing values"
  )

  checkmate::expect_string(check_duration(
    x = NULL, null_ok = FALSE
  ),
  "'NULL' cannot be 'NULL'"
  )

  checkmate::expect_string(check_duration(
    x = lubridate::dhours(1), lower = lubridate::dhours(2)
  ),
  "Element 1 is not <= "
  )

  checkmate::expect_string(check_duration(
    x = lubridate::dhours(1), upper = lubridate::dhours(0)
  ),
  "Element 1 is not >= "
  )

  checkmate::expect_string(check_duration(
    x = c(1, 1)
  ),
  "Must be of type 'Duration', not 'numeric'"
  )

  expect_true(check_duration(
    x = c(lubridate::dhours(1), lubridate::dhours(1)))
  )

  expect_true(check_duration(x = NULL, null_ok = TRUE))

  expect_equal(assert_duration(
    x = c(lubridate::dhours(1), lubridate::dhours(1))
  ),
  c(lubridate::dhours(1), lubridate::dhours(1))
  )

  expect_error(assert_duration(
    x = c(1, 1)
  ),
  "Assertion on 'c\\(1, 1\\)' failed"
  )
})
