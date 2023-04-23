
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

