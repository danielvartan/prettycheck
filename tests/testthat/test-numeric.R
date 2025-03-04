test_that("*_numeric() | General test", {
  as.integer(1) |>
    test_numeric() |>
    expect_true()

  as.double(1) |>
    test_numeric() |>
    expect_true()

  as.numeric(1) |>
    test_numeric() |>
    expect_true()

  c(1, NA) |>
    test_numeric(any_missing = TRUE) |>
    expect_true()

  NULL |>
    test_numeric(null_ok = TRUE) |>
    expect_true()

  lubridate::dhours() |>
    test_numeric() |>
    expect_false()

  letters |>
    test_numeric() |>
    expect_false()

  datasets::iris |>
    test_numeric() |>
    expect_false()
})
