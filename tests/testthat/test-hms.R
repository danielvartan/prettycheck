test_that("*_hms() | General test", {
  hms::hms(1) |>
    test_hms() |>
    expect_true()

  c(hms::hms(1), NA) |>
    test_hms(any_missing = TRUE) |>
    expect_true()

  NULL |>
    test_hms(null_ok = TRUE) |>
    expect_true()

  "a" |>
    test_hms() |>
    expect_false()

  1 |>
    test_hms() |>
    expect_false()

  lubridate::hours() |>
    test_hms() |>
    expect_false()

  lubridate::dhours() |>
    test_hms() |>
    expect_false()

  datasets::iris |>
    test_hms() |>
    expect_false()

  c(lubridate::dhours(1), NA) |>
    test_hms(any_missing = FALSE) |>
    expect_false()

  NULL |>
    test_hms(null_ok = FALSE) |>
    expect_false()

  hms::hms(1) |>
    test_hms(lower = hms::hms(2)) |>
    expect_false()

  hms::hms(1) |>
    test_hms(upper = hms::hms(0)) |>
    expect_false()

  c(1, NA) |>
    check_hms(any_missing = FALSE) |>
    checkmate::expect_string("'c\\(1, NA\\)' cannot have missing values")

  NULL |>
    check_hms(null_ok = FALSE) |>
    checkmate::expect_string("'NULL' cannot be 'NULL'")

  hms::hms(1) |>
    check_hms(lower = hms::hms(2)) |>
    checkmate::expect_string("Element 1 is not <= ")

  hms::hms(1) |>
    check_hms(upper = hms::hms(0)) |>
    checkmate::expect_string("Element 1 is not >= ")

  c(1, 1) |>
    check_hms() |>
    checkmate::expect_string("Must be of type 'hms', not 'numeric'")

  c(hms::hms(1), hms::hms(1)) |>
    check_hms() |>
    expect_true()

  NULL |>
    check_hms(null_ok = TRUE) |>
    expect_true()

  c(hms::hms(1), hms::hms(1)) |>
    assert_hms() |>
    expect_equal(c(hms::hms(1), hms::hms(1)))

  c(1, 1) |>
    assert_hms() |>
    expect_error("Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_hms() | Error test", {
  # checkmate::assert_flag(any_missing)
  hms::hms(1) |>
    test_hms(any_missing = 1) |>
    expect_error()

  hms::hms(1) |>
    check_hms(any_missing = 1) |>
    expect_error()

  # checkmate::assert_flag(null_ok)
  hms::hms(1) |>
    test_hms(null_ok = 1) |>
    expect_error()

  hms::hms(1) |>
    check_hms(null_ok = 1) |>
    expect_error()
})
