
test_that("`mode_is_trivial()` works correctly", {
  expect_equal(mode_is_trivial(x1 ), FALSE)
  expect_equal(mode_is_trivial(x2 ), NA)
  expect_equal(mode_is_trivial(c(x2, NA)), FALSE)
  expect_equal(mode_is_trivial(c(x2, NA, NA)), NA)
  expect_equal(mode_is_trivial(x3 ), FALSE)
  expect_equal(mode_is_trivial(x4 ), FALSE)
  expect_equal(mode_is_trivial(x5 ), FALSE)
  expect_equal(mode_is_trivial(x6 ), FALSE)
  expect_equal(mode_is_trivial(x7 ), FALSE)
  expect_equal(mode_is_trivial(x8 ), NA)
  expect_equal(mode_is_trivial(x9 ), NA)
  expect_equal(mode_is_trivial(x10), NA)
  expect_equal(mode_is_trivial(x11), TRUE)
  expect_equal(mode_is_trivial(x12), FALSE)
  expect_equal(mode_is_trivial(x13), TRUE)
  expect_equal(mode_is_trivial(x14), NA)
  expect_equal(mode_is_trivial(x15), NA)
  expect_equal(mode_is_trivial(x16), FALSE)
  expect_equal(mode_is_trivial(x17), NA)
  # Check variants of `x5`:
  expect_equal(mode_is_trivial(c(1, 2, NA)), NA)
  expect_equal(mode_is_trivial(c("a", "b", "b", NA)), NA)
  expect_equal(mode_is_trivial(c("a", "a", "b", "b", NA)), FALSE)
  # Other issue:
  expect_equal(mode_is_trivial(c(1, 1, 1, 2, rep(NA, 5))), NA)
})


test_that("`mode_is_trivial()` works correctly with non-`NULL` `max_unique", {
  expect_equal(mode_is_trivial(x1 , max_unique = 3), FALSE)
  expect_equal(mode_is_trivial(x2 , max_unique = 3), NA)
  expect_equal(mode_is_trivial(c(x2, NA), max_unique = 3), FALSE)
  expect_equal(mode_is_trivial(c(x2, NA, NA), max_unique = 3), NA)
  expect_equal(mode_is_trivial(c(1, 1, 1, 2, rep(NA, 5)), max_unique = "known"), FALSE)
})

