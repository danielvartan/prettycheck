test_that("assert_identical() | general test", {
  assert_identical(1) |> expect_error()
  assert_identical(1, c(1, 1), type = "value") |> expect_error()
  assert_identical(1, 1, type = "value") |> checkmate::expect_list()

  assert_identical(1, c(2, 2), type = "length") |> expect_error()
  assert_identical(1, 2, type = "length") |> checkmate::expect_list()

  assert_identical(1, "a", type = "class") |> expect_error()
  assert_identical(1, 3, type = "class") |> checkmate::expect_list()

  assert_identical(NULL, NULL) |> checkmate::expect_list()
  assert_identical(1, NA) |> expect_error()
})
