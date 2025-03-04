test_that("*_posixt() | general test", {
  lubridate::as_datetime(1) |>
    test_posixt() |>
    expect_true()

  lubridate::as_datetime(1) |>
    as.POSIXlt() |>
    test_posixt() |>
    expect_true()

  c(lubridate::as_datetime(1), NA) |>
    test_posixt(any_missing = TRUE) |>
    expect_true()

  NULL |>
    test_posixt(null_ok = TRUE) |>
    expect_true()

  "a" |>
    test_posixt() |>
    expect_false()

  1 |>
    test_posixt() |>
    expect_false()

  lubridate::hours() |>
    test_posixt() |>
    expect_false()

  hms::hms(1) |>
    test_posixt() |>
    expect_false()

  datasets::iris |>
    test_posixt() |>
    expect_false()

  c(lubridate::as_datetime(1), NA) |>
    test_posixt(any_missing = FALSE) |>
    expect_false()

  NULL |>
    test_posixt(null_ok = FALSE) |>
    expect_false()

  lubridate::as_datetime(1) |>
    test_posixt(lower = lubridate::as_datetime(2)) |>
    expect_false()

  lubridate::as_datetime(1) |>
    test_posixt(upper = lubridate::as_datetime(0)) |>
    expect_false()

  c(1, NA) |>
    check_posixt(any_missing = FALSE) |>
    checkmate::expect_string("'c\\(1, NA\\)' cannot have missing values")

  NULL |>
    check_posixt(null_ok = FALSE) |>
    checkmate::expect_string("'NULL' cannot be 'NULL'")

  lubridate::as_datetime(1) |>
    check_posixt(lower = lubridate::as_datetime(2)) |>
    checkmate::expect_string("Element 1 is not <= ")

  lubridate::as_datetime(1) |>
    check_posixt(upper = lubridate::as_datetime(0)) |>
    checkmate::expect_string("Element 1 is not >= ")

  c(1, 1) |>
    check_posixt() |>
    checkmate::expect_string("Must be of type 'POSIXct' or 'POSIXlt', ")

  c(lubridate::as_datetime(1), lubridate::as_datetime(1)) |>
    check_posixt() |>
    expect_true()

  NULL |>
    check_posixt(null_ok = TRUE) |>
    expect_true()

  c(lubridate::as_datetime(1), lubridate::as_datetime(1)) |>
    assert_posixt() |>
    expect_equal(c(lubridate::as_datetime(1), lubridate::as_datetime(1)))

  c(1, 1) |>
    assert_posixt() |>
    expect_error("Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_posixt() | Error test", {
  lubridate::as_datetime(1) |>
    test_posixt(any_missing = 1) |>
    expect_error()

  lubridate::as_datetime(1) |>
    check_posixt(any_missing = 1) |>
    expect_error()

  lubridate::as_datetime(1) |>
    test_posixt(null_ok = 1) |>
    expect_error()

  lubridate::as_datetime(1) |>
    check_posixt(null_ok = 1) |>
    expect_error()
})
