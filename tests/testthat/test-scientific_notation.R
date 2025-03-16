test_that("*_scientific_notation() | General test", {
  opt <- getOption("scipen")

  x <- 0.00000000000000000000000001

  options(scipen = 0)
  x |>
    test_scientific_notation() |>
    expect_true()

  options(scipen = 999)
  x |>
    test_scientific_notation() |>
    expect_false()

  x |>
    check_scientific_notation() |>
    checkmate::expect_string("'x' is not formatted in scientific notation")

  NULL |>
    check_scientific_notation(null_ok = TRUE) |>
    expect_true()

  NULL |>
    check_scientific_notation(null_ok = FALSE) |>
    checkmate::expect_string("'NULL' cannot be 'NULL'")

  options(scipen = 0)

  c(x, NA) |>
    check_scientific_notation(any_missing = TRUE) |>
    expect_true()

  c(1, NA) |>
    check_scientific_notation(any_missing = FALSE) |>
    checkmate::expect_string("'c\\(1, NA\\)' cannot have missing values")

  x |>
    assert_scientific_notation() |>
    expect_equal(x)

  options(scipen = 999)

  x |>
    assert_scientific_notation() |>
    expect_error("Assertion on 'x' failed")

  options(scipen = opt)
})

test_that("*_scientific_notation() | Error test", {
  # assert_numeric(x, any_missing = TRUE, null_ok = TRUE)
  test_scientific_notation(
    x = "a",
    any_missing = TRUE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) |>
    expect_error()

  # assert_flag(any_missing)
  test_scientific_notation(
    x = 1,
    any_missing = "a",
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) |>
    expect_error()

  # assert_flag(null_ok)
  test_scientific_notation(
    x = 1,
    any_missing = TRUE,
    null_ok = "a",
    .names = deparse(substitute(x))
  ) |>
    expect_error()

  # assert_string(.names)
  test_scientific_notation(
    x = 1,
    any_missing = TRUE,
    null_ok = FALSE,
    .names = 1
  ) |>
    expect_error()
})
