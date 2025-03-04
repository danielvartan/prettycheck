test_that("*_interval() | General test", {
  int <- lubridate::interval(
    as.POSIXct("2020-01-01 00:00:00"),
    as.POSIXct("2020-01-02 00:00:00")
  )

  int_lower <- lubridate::interval(
    as.POSIXct("2020-01-01 01:00:00"),
    as.POSIXct("2020-01-02 00:00:00")
  )

  int_upper <- lubridate::interval(
    as.POSIXct("2020-01-01 00:00:00"),
    as.POSIXct("2020-01-02 01:00:00")
  )

  int |>
    test_interval() |>
    expect_true()

  c(int, lubridate::as.interval(NA)) |>
    test_interval(any_missing = TRUE) |>
    expect_true()

  NULL |>
    test_interval(null_ok = TRUE) |>
    expect_true()

  int |>
    test_interval(lower = int_lower) |>
    expect_true()

  int |>
    test_interval(upper = int_upper) |>
    expect_true()

  "a" |>
    test_interval() |>
    expect_false()

  1 |>
    test_interval() |>
    expect_false()

  lubridate::hours() |>
    test_interval() |>
    expect_false()

  hms::hms(1) |>
    test_interval() |>
    expect_false()

  datasets::iris |>
    test_interval() |>
    expect_false()

  c(int, NA) |>
    test_interval(any_missing = FALSE) |>
    expect_false()

  NULL |>
    test_interval(null_ok = FALSE) |>
    expect_false()

  int |>
    test_interval(lower = int_upper) |>
    expect_false()

  int |>
    test_interval(upper = int_lower) |>
    expect_false()

  c(1, NA) |>
    check_interval(any_missing = FALSE) |>
    checkmate::expect_string(
      pattern = "'c\\(1, NA\\)' cannot have missing values"
    )

  NULL |>
    check_interval(null_ok = FALSE) |>
    checkmate::expect_string(pattern = "'NULL' cannot be 'NULL'")

  int |>
    check_interval(lower = int_upper) |>
    checkmate::expect_string(pattern = "Element 1 is not >= ")

  int |>
    check_interval(upper = int_lower) |>
    checkmate::expect_string(pattern = "Element 1 is not <= ")

  c(1, 1) |>
    check_interval() |>
    checkmate::expect_string(pattern = "Must be of type 'Interval'")

  c(int, int_lower) |>
    check_interval() |>
    expect_true()

  NULL |>
    check_interval(null_ok = TRUE) |>
    expect_true()

  c(int, int_lower) |>
    assert_interval() |>
    expect_equal(c(int, int_lower))

  c(1, 1) |>
    assert_interval() |>
    expect_error("Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_interval() | Error test", {
  # checkmate::assert_flag(any_missing)
  lubridate::as_datetime(1) |>
    test_interval(any_missing = 1) |>
    expect_error()

  lubridate::as_datetime(1) |>
    check_interval(any_missing = 1) |>
    expect_error()

  # checkmate::assert_flag(null_ok)
  lubridate::as_datetime(1) |>
    test_interval(null_ok = 1) |>
    expect_error()

  lubridate::as_datetime(1) |>
    check_interval(null_ok = 1) |>
    expect_error()
})
