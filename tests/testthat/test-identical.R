# Something is wrong. It's taking too much time.

# test_that("assert_identical() | general test", {
#   expect_error(assert_identical(1))
#   expect_error(assert_identical(1, c(1, 1), type = "value"))
#   expect_true(assert_identical(1, 1, type = "value"))
#
#   expect_error(assert_identical(1, c(2, 2), type = "length"))
#   expect_true(assert_identical(1, 2, type = "length"))
#
#   expect_error(assert_identical(1, "a", type = "class"))
#   expect_true(assert_identical(1, 3, type = "class"))
#
#   expect_true(assert_identical(NULL, NULL))
#   expect_error(assert_identical(1, NA))
# })
