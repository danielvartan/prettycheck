test_that("*_choice() | General test", {
  test_choice("a", letters) |> expect_true()
})
