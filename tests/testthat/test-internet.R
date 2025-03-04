test_that("test_internet() | General test", {
  mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
    mockr::with_mock(
      has_internet = function(...) TRUE,
      {
        test_internet()
      }
    )
  }

  expect_true(mock())
})
