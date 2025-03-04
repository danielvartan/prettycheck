test_that("*_temporal() | General test", {
  lubridate::dhours() |>
    test_temporal() |>
    expect_true()

  lubridate::hours() |>
    test_temporal() |>
    expect_true()

  as.difftime(1, units = "secs") |>
    test_temporal() |>
    expect_true()

  hms::hms(1) |>
    test_temporal() |>
    expect_true()

  as.Date("2000-01-01") |>
    test_temporal() |>
    expect_true()

  lubridate::as_datetime(1) |>
    test_temporal() |>
    expect_true()

  as.POSIXlt(lubridate::as_datetime(1)) |>
    test_temporal() |>
    expect_true()

  test_temporal(NULL, null_ok = TRUE) |>
    expect_true()

  1 |>
    test_temporal() |>
    expect_false()

  letters |>
    test_temporal() |>
    expect_false()

  datasets::iris |>
    test_temporal() |>
    expect_false()

  lubridate::dhours() |>
    test_temporal(rm = "Duration") |>
    expect_false()

  lubridate::hours() |>
    test_temporal(rm = "Period") |>
    expect_false()

  as.difftime(1, units = "secs") |>
    test_temporal(rm = "difftime") |>
    expect_false()

  hms::hms(1) |>
    test_temporal(rm = "hms") |>
    expect_false()

  as.Date("2000-01-01") |>
    test_temporal(rm = "Date") |>
    expect_false()

  lubridate::as_datetime(1) |>
    test_temporal(rm = "POSIXct") |>
    expect_false()

  as.POSIXlt(lubridate::as_datetime(1)) |>
    test_temporal(rm = "POSIXlt") |>
    expect_false()

  c(1, NA) |>
    test_temporal(any_missing = FALSE) |>
    expect_false()

  NULL |>
    test_temporal(null_ok = FALSE) |>
    expect_false()

  c(1, 1) |>
    check_temporal() |>
    checkmate::expect_string(pattern = "Must be a temporal object ")

  c(1, NA) |>
    check_temporal(any_missing = FALSE) |>
    checkmate::expect_string(pattern = "'c\\(1, NA\\)' cannot have missing ")

  NULL |>
    check_temporal(null_ok = FALSE) |>
    checkmate::expect_string(pattern = "'NULL' cannot be 'NULL'")

  c(lubridate::hours(1), lubridate::hours(1)) |>
    check_temporal() |>
    expect_true()

  NULL |>
    check_temporal(null_ok = TRUE) |>
    expect_true()

  c(lubridate::hours(1), lubridate::hours(1)) |>
    assert_temporal() |>
    expect_equal(c(lubridate::hours(1), lubridate::hours(1)))

  c(1, 1) |>
    assert_temporal() |>
    expect_error("Assertion on 'c\\(1, 1\\)' failed")
})
