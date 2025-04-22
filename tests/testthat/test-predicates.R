
test_that("`is_unimodal()` works correctly", {
  expect_true(is_unimodal(x1))
  expect_equal(is_unimodal(x2), NA)
  expect_true(is_unimodal(x3))
  expect_false(is_unimodal(x4))
  expect_equal(is_unimodal(x5), NA)
  expect_true(is_unimodal(x6))
  expect_false(is_unimodal(x7))
  expect_equal(is_unimodal(x8), NA)
  expect_equal(is_unimodal(x9), NA)
  expect_true(is_unimodal(x10))
  expect_equal(is_unimodal(x11), NA)
  expect_equal(is_unimodal(x12), NA)
  expect_equal(is_unimodal(x13), NA)
  expect_equal(is_unimodal(x14), NA)
  expect_equal(is_unimodal(x15), NA)
  expect_equal(is_unimodal(x16), NA)
  expect_equal(is_unimodal(x17), NA)
})

# Don't need to test against all of the same vectors again:
test_that("`is_multimodal()` works correctly", {
  expect_false(is_multimodal(x1))
  expect_equal(is_multimodal(x2), NA)
  expect_false(is_multimodal(x3))
  expect_true(is_multimodal(x4))
  expect_equal(is_multimodal(x5), NA)
})


test_that("`is_uniform()` works correctly", {
  expect_equal(is_uniform(x1 ), FALSE)
  expect_equal(is_uniform(x2 ), NA)
  expect_equal(is_uniform(c(x2, NA)), FALSE)
  expect_equal(is_uniform(c(x2, NA, NA)), NA)
  expect_equal(is_uniform(x3 ), FALSE)
  expect_equal(is_uniform(x4 ), FALSE)
  expect_equal(is_uniform(x5 ), FALSE)
  expect_equal(is_uniform(x6 ), FALSE)
  expect_equal(is_uniform(x7 ), FALSE)
  expect_equal(is_uniform(x8 ), NA)
  expect_equal(is_uniform(x9 ), NA)
  expect_equal(is_uniform(x10), NA)
  expect_equal(is_uniform(x11), TRUE)
  expect_equal(is_uniform(x12), FALSE)
  expect_equal(is_uniform(x13), TRUE)
  expect_equal(is_uniform(x14), NA)
  expect_equal(is_uniform(x15), NA)
  expect_equal(is_uniform(x16), FALSE)
  expect_equal(is_uniform(x17), NA)
  # Check variants of `x5`:
  expect_equal(is_uniform(c(1, 2, NA)), NA)
  expect_equal(is_uniform(c("a", "b", "b", NA)), NA)
  expect_equal(is_uniform(c("a", "a", "b", "b", NA)), FALSE)
  # Other issue:
  expect_equal(is_uniform(c(1, 1, 1, 2, rep(NA, 5))), NA)
})


test_that("`is_uniform()` works correctly with non-`NULL` `max_unique", {
  expect_equal(is_uniform(x1 , max_unique = 3), FALSE)
  expect_equal(is_uniform(x2 , max_unique = 3), NA)
  expect_equal(is_uniform(c(x2, NA), max_unique = 3), FALSE)
  expect_equal(is_uniform(c(x2, NA, NA), max_unique = 3), NA)
  expect_equal(is_uniform(c(1, 1, 1, 2, rep(NA, 5)), max_unique = "known"), FALSE)
})

