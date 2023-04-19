# # Test vectors:
# x1 <- c(7, 8, 8, 9, 9, 9)
# x2 <- c(1, 1, 2, 2, 2, 2, NA, NA, NA, NA)
# x3 <- c(7, 7, 7, 7, 8, 8, NA)
# x4 <- c("a", "a", "b", "b", "c", "d", "e")
# x5 <- c(1, 1, 2, 2, NA)
# x6 <- c(3, 4, 4, 5, 5, 5)
# x7 <- c("x", "y", "y", "z", "z")
# x8 <- c(1, 1, 2, NA)
# x9 <- c(2, 1, 1, NA)
# x10 <- c(1, 1, NA)
# x11 <- c(1, NA)
# x12 <- c("a", "a", "a", "b", "b", "c", "d", "e", NA)
# x13 <- c(NA, 1)
# x14 <- c(1, 1, 2, 2, 3, NA)
# x15 <- c(1, NA, NA)
# x16 <- c(1, 1, 1, 2, 2, 3, 3, NA)
# x17 <- c(1, 1, 2, 2, NA, NA)


test_that("`mode_frequency()` is right when no `NA`s are present", {
  expect_equal(mode_frequency(x1), 3L)
  expect_equal(mode_frequency(x4), 2L)
  expect_equal(mode_frequency(x6), 3L)
  expect_equal(mode_frequency(x7), 2L)
})

# Note: By default (`na.rm = FALSE`), `mode_frequency()` returns `NA_integer_`
# whenever any value is missing.

test_that("`mode_frequency()` is right with some `NA` input
          but non-`NA` output", {
  expect_equal(mode_frequency(x3) , NA_integer_)
  expect_equal(mode_frequency(x10), NA_integer_)
})

test_that("`mode_frequency()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_frequency(x2) , NA_integer_)
  expect_equal(mode_frequency(x3) , NA_integer_)
  expect_equal(mode_frequency(x5) , NA_integer_)
  expect_equal(mode_frequency(x8) , NA_integer_)
  expect_equal(mode_frequency(x9) , NA_integer_)
  expect_equal(mode_frequency(x10), NA_integer_)
  expect_equal(mode_frequency(x11), NA_integer_)
  expect_equal(mode_frequency(x12), NA_integer_)
  expect_equal(mode_frequency(x13), NA_integer_)
  expect_equal(mode_frequency(x14), NA_integer_)
  expect_equal(mode_frequency(x15), NA_integer_)
  expect_equal(mode_frequency(x16), NA_integer_)
  expect_equal(mode_frequency(x17), NA_integer_)
})

test_that("`mode_frequency()` is right with some `NA` input
          and `na.rm = TRUE`", {
  expect_equal(mode_frequency(x2 , na.rm = TRUE), 4L)
  expect_equal(mode_frequency(x5 , na.rm = TRUE), 2L)
  expect_equal(mode_frequency(x8 , na.rm = TRUE), 2L)
  expect_equal(mode_frequency(x9 , na.rm = TRUE), 2L)
  expect_equal(mode_frequency(x11, na.rm = TRUE), 1L)
  expect_equal(mode_frequency(x12, na.rm = TRUE), 3L)
  expect_equal(mode_frequency(x13, na.rm = TRUE), 1L)
  expect_equal(mode_frequency(x14, na.rm = TRUE), 2L)
  expect_equal(mode_frequency(x15, na.rm = TRUE), 1L)
  expect_equal(mode_frequency(x16, na.rm = TRUE), 3L)
  expect_equal(mode_frequency(x17, na.rm = TRUE), 2L)
})


test_that("`mode_frequency_range()` works correctly", {
  expect_equal(mode_frequency_range(x1 ), c(3L, 3L))
  expect_equal(mode_frequency_range(x2 ), c(4L, 8L))
  expect_equal(mode_frequency_range(x3 ), c(4L, 5L))
  expect_equal(mode_frequency_range(x4 ), c(2L, 2L))
  expect_equal(mode_frequency_range(x5 ), c(2L, 3L))
  expect_equal(mode_frequency_range(x6 ), c(3L, 3L))
  expect_equal(mode_frequency_range(x7 ), c(2L, 2L))
  expect_equal(mode_frequency_range(x8 ), c(2L, 3L))
  expect_equal(mode_frequency_range(x9 ), c(2L, 3L))
  expect_equal(mode_frequency_range(x10), c(2L, 3L))
  expect_equal(mode_frequency_range(x11), c(1L, 2L))
  expect_equal(mode_frequency_range(x12), c(3L, 4L))
  expect_equal(mode_frequency_range(x13), c(1L, 2L))
  expect_equal(mode_frequency_range(x14), c(2L, 3L))
  expect_equal(mode_frequency_range(x15), c(1L, 3L))
  expect_equal(mode_frequency_range(x16), c(3L, 4L))
  expect_equal(mode_frequency_range(x17), c(2L, 4L))
})

