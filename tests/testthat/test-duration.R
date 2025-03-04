test_that("*_duration() | General test", {
  test_duration(x = lubridate::dhours(1)) |> expect_true()

  test_duration(x = c(lubridate::dhours(1), NA), any_missing = TRUE) |>
    expect_true()

  test_duration(x = NULL, null_ok = TRUE) |> expect_true()
  test_duration(x = "a") |> expect_false()
  test_duration(x = 1) |> expect_false()
  test_duration(x = lubridate::hours()) |> expect_false()
  test_duration(x = hms::hms(1)) |> expect_false()
  test_duration(x = datasets::iris) |> expect_false()

  test_duration(x = c(lubridate::dhours(1), NA), any_missing = FALSE) |>
    expect_false()

  test_duration(x = NULL, null_ok = FALSE) |> expect_false()

  test_duration(x = lubridate::dhours(1), lower = lubridate::dhours(2)) |>
    expect_false()

  test_duration(x = lubridate::dhours(1), upper = lubridate::dhours(0)) |>
    expect_false()

  check_duration(x = c(1, NA), any_missing = FALSE) |>
    checkmate::expect_string("'c\\(1, NA\\)' cannot have missing values")

  check_duration(x = NULL, null_ok = FALSE) |>
    checkmate::expect_string("'NULL' cannot be 'NULL'")

  check_duration(x = lubridate::dhours(1), lower = lubridate::dhours(2)) |>
    checkmate::expect_string("Element 1 is not <= ")

  check_duration(x = lubridate::dhours(1), upper = lubridate::dhours(0)) |>
    checkmate::expect_string("Element 1 is not >= ")

  check_duration(x = c(1, 1)) |>
    checkmate::expect_string("Must be of type 'Duration', not 'numeric'")

  check_duration(x = c(lubridate::dhours(1), lubridate::dhours(1))) |>
    expect_true()

  check_duration(x = NULL, null_ok = TRUE) |> expect_true()

  assert_duration(x = c(lubridate::dhours(1), lubridate::dhours(1))) |>
    expect_equal(c(lubridate::dhours(1), lubridate::dhours(1)))

  assert_duration(x = c(1, 1)) |>
    expect_error("Assertion on 'c\\(1, 1\\)' failed")
})
