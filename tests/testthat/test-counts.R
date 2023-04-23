
test_that("`mode_count()` is right when no `NA`s are present", {
  expect_equal(mode_count(x1), 1)
  expect_equal(mode_count(x4), 2)
  expect_equal(mode_count(x6), 1)
  expect_equal(mode_count(x7), 2)
})

test_that("`mode_count()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_count(x3) , 1L)
  expect_equal(mode_count(x10), 1L)
})

test_that("`mode_count()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_count(x2) , NA_integer_)
  expect_equal(mode_count(x5) , NA_integer_)
  expect_equal(mode_count(x8) , NA_integer_)
  expect_equal(mode_count(x9) , NA_integer_)
  expect_equal(mode_count(x11), NA_integer_)
  expect_equal(mode_count(x12), NA_integer_)
  expect_equal(mode_count(x13), NA_integer_)
  expect_equal(mode_count(x14), NA_integer_)
  expect_equal(mode_count(x15), NA_integer_)
  expect_equal(mode_count(x16), NA_integer_)
  expect_equal(mode_count(x17), NA_integer_)
})

test_that("`mode_count()` is right with some `NA` input and `na.rm = TRUE`", {
  expect_equal(mode_count(x2 , na.rm = TRUE), 1L)
  expect_equal(mode_count(x5 , na.rm = TRUE), 2L)
  expect_equal(mode_count(x8 , na.rm = TRUE), 1L)
  expect_equal(mode_count(x9 , na.rm = TRUE), 1L)
  expect_equal(mode_count(x11, na.rm = TRUE), 1L)
  expect_equal(mode_count(x12, na.rm = TRUE), 1L)
  expect_equal(mode_count(x13, na.rm = TRUE), 1L)
  expect_equal(mode_count(x14, na.rm = TRUE), 2L)
  expect_equal(mode_count(x15, na.rm = TRUE), 1L)
  expect_equal(mode_count(x16, na.rm = TRUE), 1L)
  expect_equal(mode_count(x17, na.rm = TRUE), 2L)
})


test_that("`mode_count_range()` works correctly", {
  expect_equal(mode_count_range(x1 ), c(1L, 1L))
  expect_equal(mode_count_range(x2 ), c(1L, 2L))
  expect_equal(mode_count_range(x3 ), c(1L, 1L))
  expect_equal(mode_count_range(x4 ), c(2L, 2L))
  expect_equal(mode_count_range(x5 ), c(1L, 2L))
  expect_equal(mode_count_range(x6 ), c(1L, 1L))
  expect_equal(mode_count_range(x7 ), c(2L, 2L))
  expect_equal(mode_count_range(x8 ), c(1L, 2L))
  expect_equal(mode_count_range(x9 ), c(1L, 2L))
  expect_equal(mode_count_range(x10), c(1L, 1L))
  expect_equal(mode_count_range(x11), c(1L, 2L))
  expect_equal(mode_count_range(x12), c(1L, 2L))
  expect_equal(mode_count_range(x13), c(1L, 2L))
  expect_equal(mode_count_range(x14), c(1L, 3L))
  expect_equal(mode_count_range(x15), c(1L, 3L))
  expect_equal(mode_count_range(x16), c(1L, 2L))
  expect_equal(mode_count_range(x17), c(1L, 3L))
})

test_that("`mode_count_range()` works correctly with
          `max_unique = \"known\"`", {
  expect_equal(mode_count_range(x1 , max_unique = "known"), c(1L, 1L))
  expect_equal(mode_count_range(x2 , max_unique = "known"), c(1L, 2L))
  expect_equal(mode_count_range(x3 , max_unique = "known"), c(1L, 1L))
  expect_equal(mode_count_range(x4 , max_unique = "known"), c(2L, 2L))
  # `x5` has a different expectation here: Since the `NA` is known to be either
  # `1` or `2`, it must tip the balance either way, leading to a single mode!
  expect_equal(mode_count_range(x5 , max_unique = "known"), c(1L, 1L))
  expect_equal(mode_count_range(x6 , max_unique = "known"), c(1L, 1L))
  expect_equal(mode_count_range(x7 , max_unique = "known"), c(2L, 2L))
  expect_equal(mode_count_range(x8 , max_unique = "known"), c(1L, 2L))
  expect_equal(mode_count_range(x9 , max_unique = "known"), c(1L, 2L))
  expect_equal(mode_count_range(x10, max_unique = "known"), c(1L, 1L))
  # `x11` has only one known value, so if it's exclusive to the known values,
  # the `NA` must represent that value, as well:
  expect_equal(mode_count_range(x11, max_unique = "known"), c(1L, 1L))
  expect_equal(mode_count_range(x12, max_unique = "known"), c(1L, 2L))
  # (See `x11`.)
  expect_equal(mode_count_range(x13, max_unique = "known"), c(1L, 1L))
  expect_equal(mode_count_range(x14, max_unique = "known"), c(1L, 3L))
  # (See `x11`.)
  expect_equal(mode_count_range(x15, max_unique = "known"), c(1L, 1L))
  expect_equal(mode_count_range(x16, max_unique = "known"), c(1L, 2L))
  # As in `x11`, except there are two known values, so the `NA`s must be
  # fully distributed across them instead of forming a new mode:
  expect_equal(mode_count_range(x17, max_unique = "known"), c(1L, 2L))
})

test_that("`mode_count_range()` works correctly with non-`NULL` `max_unique`", {
  x <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
  expect_equal(mode_count_range(c(x, rep(NA, 10L))), c(1L, 6L))
  expect_equal(mode_count_range(c(x, rep(NA, 10L)), max_unique = 5), c(1L, 4L))
  expect_equal(mode_count_range(c(x, rep(NA,  9L)), max_unique = 5), c(1L, 3L))
  expect_equal(mode_count_range(c(x, rep(NA,  8L)), max_unique = 5), c(1L, 2L))
  expect_equal(mode_count_range(c(x, rep(NA,  7L)), max_unique = 5), c(1L, 1L))
  expect_equal(mode_count_range(c(x, rep(NA,  6L)), max_unique = 5), c(1L, 5L))
})

test_that("the warnings for factors in `mode_count_range()` work correctly", {
  x <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
  expect_warning(mode_count_range(as.factor(x), max_unique = 3))
  # These two tests check for the presence of absence of multiple warnings (and,
  # in the second test, an error); hence the nested expectations:
  expect_no_warning(expect_warning(mode_count_range(as.factor(x), max_unique = 3)))
  expect_error(expect_warning(expect_warning(mode_count_range(as.factor(x), max_unique = 1))))
})

