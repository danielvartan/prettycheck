test_that("assert_internet() | general test", {
  mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
    mockr::with_mock(
      has_internet = function(...) TRUE,
      {
        assert_internet()
      }
    )
  }

  expect_true(mock())

  mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
    mockr::with_mock(
      has_internet = function(...) FALSE,
      {
        assert_internet()
      }
    )
  }

  expect_error(mock())
})
