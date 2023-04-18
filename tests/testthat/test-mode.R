# Test vectors:
x1 <- c(7, 8, 8, 9, 9, 9)
x2 <- c(1, 1, 2, 2, 2, 2, NA, NA, NA, NA)
x3 <- c(7, 7, 7, 7, 8, 8, NA)
x4 <- c("a", "a", "b", "b", "c", "d", "e")
x5 <- c(1, 1, 2, 2, NA)
x6 <- c(3, 4, 4, 5, 5, 5)
x7 <- c("x", "y", "y", "z", "z")
x8 <- c(1, 1, 2, NA)
x9 <- c(2, 1, 1, NA)
x10 <- c(1, 1, NA)
x11 <- c(1, NA)
x12 <- c("a", "a", "a", "b", "b", "c", "d", "e", NA)
x13 <- c(NA, 1)
x14 <- c(1, 1, 2, 2, 3, NA)
x15 <- c(1, NA, NA)
x16 <- c(1, 1, 1, 2, 2, 3, 3, NA)
x17 <- c(1, 1, 2, 2, NA, NA)

# # Test interactively:
# cat(paste0("mode_count_range(x", 1:17, ")\n"))


# Actual modes, without `NA`s ---------------------------------------------

test_that("`mode_first()` is right when no `NA`s are present", {
  expect_equal(mode_first(x1), 9)
  expect_equal(mode_first(x4), "a")
  expect_equal(mode_first(x6), 5)
  expect_equal(mode_first(x7), "y")
})

test_that("`mode_all()` is right when no `NA`s are present", {
  expect_equal(mode_all(x1), 9)
  expect_equal(mode_all(x4), c("a", "b"))
  expect_equal(mode_all(x6), 5)
  expect_equal(mode_all(x7), c("y", "z"))
})

test_that("`mode_single()` is right when no `NA`s are present", {
  expect_equal(mode_single(x1), 9)
  expect_equal(mode_single(x4), NA_character_)
  expect_equal(mode_single(x6), 5)
  expect_equal(mode_single(x7), NA_character_)
})

test_that("`mode_count()` is right when no `NA`s are present", {
  expect_equal(mode_count(x1), 1)
  expect_equal(mode_count(x4), 2)
  expect_equal(mode_count(x6), 1)
  expect_equal(mode_count(x7), 2)
})

test_that("`mode_frequency()` is right when no `NA`s are present", {
  expect_equal(mode_frequency(x1), 3L)
  expect_equal(mode_frequency(x4), 2L)
  expect_equal(mode_frequency(x6), 3L)
  expect_equal(mode_frequency(x7), 2L)
})



# Actual modes, with `NA`s ------------------------------------------------

# 1. `mode_first()`

test_that("`mode_first()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_first(x3) , 7)
  expect_equal(mode_first(x8) , 1)
  expect_equal(mode_first(x9) , NA_real_)
  expect_equal(mode_first(x10), 1)
  expect_equal(mode_first(x11), 1)
  expect_equal(mode_first(x12), "a")
  expect_equal(mode_first(x16), 1)
})

test_that("`mode_first()` is right with some `NA` input, non-`NA` output,
          and `first_known = FALSE`", {
  expect_equal(mode_first(x3 , first_known = FALSE), 7)
  expect_equal(mode_first(x8 , first_known = FALSE), 1)
  expect_equal(mode_first(x10, first_known = FALSE), 1)
  expect_equal(mode_first(x11, first_known = FALSE), 1)
  expect_equal(mode_first(x12, first_known = FALSE), "a")
  expect_equal(mode_first(x13, first_known = FALSE), NA_real_)
  expect_equal(mode_first(x16, first_known = FALSE), 1)
})

test_that("`mode_first()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_first(x2 ), NA_real_)
  expect_equal(mode_first(x5 ), NA_real_)
  expect_equal(mode_first(x14), NA_real_)
  expect_equal(mode_first(x15), NA_real_)
  expect_equal(mode_first(x17), NA_real_)
})

test_that("`mode_first()` is right with some `NA` input and `na.rm = TRUE`", {
  expect_equal(mode_first(x2 , na.rm = TRUE), 2)
  expect_equal(mode_first(x5 , na.rm = TRUE), 1)
  expect_equal(mode_first(x14, na.rm = TRUE), 1)
  expect_equal(mode_first(x15, na.rm = TRUE), 1)
  expect_equal(mode_first(x16, na.rm = TRUE), 1)
  expect_equal(mode_first(x17, na.rm = TRUE), 1)
})

test_that("`mode_first()` is right with some `NA` input, `NA` output,
          and `first_known = FALSE`", {
  expect_equal(mode_first(x2 , first_known = FALSE), NA_real_)
  expect_equal(mode_first(x5 , first_known = FALSE), NA_real_)
  expect_equal(mode_first(x9 , first_known = FALSE), NA_real_)
  expect_equal(mode_first(x13, first_known = FALSE), NA_real_)
  expect_equal(mode_first(x14, first_known = FALSE), NA_real_)
  expect_equal(mode_first(x15, first_known = FALSE), NA_real_)
  expect_equal(mode_first(x15, first_known = FALSE), NA_real_)
})

test_that("`mode_first()` is right with some `NA` input, `na.rm = TRUE`,
          and `first_known = FALSE`", {
  expect_equal(mode_first(x2 , na.rm = TRUE, first_known = FALSE), 2)
  expect_equal(mode_first(x5 , na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x9 , na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x13, na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x14, na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x15, na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x16, na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x17, na.rm = TRUE, first_known = FALSE), 1)
})


# 2. `mode_all()`

test_that("`mode_all()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_all(x3) , 7)
  expect_equal(mode_all(x10), 1)
})

test_that("`mode_all()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_all(x2) , NA_real_)
  expect_equal(mode_all(x5) , NA_real_)
  expect_equal(mode_all(x8) , NA_real_)
  expect_equal(mode_all(x9) , NA_real_)
  expect_equal(mode_all(x11), NA_real_)
  expect_equal(mode_all(x12), NA_character_)
  expect_equal(mode_all(x13), NA_real_)
  expect_equal(mode_all(x14), NA_real_)
  expect_equal(mode_all(x15), NA_real_)
  expect_equal(mode_all(x16), NA_real_)
  expect_equal(mode_all(x17), NA_real_)
})

test_that("`mode_all()` is right with some `NA` input and `na.rm = TRUE`", {
  expect_equal(mode_all(x2 , na.rm = TRUE), 2)
  expect_equal(mode_all(x5 , na.rm = TRUE), c(1, 2))
  expect_equal(mode_all(x8 , na.rm = TRUE), 1)
  expect_equal(mode_all(x9 , na.rm = TRUE), 1)
  expect_equal(mode_all(x11, na.rm = TRUE), 1)
  expect_equal(mode_all(x12, na.rm = TRUE), "a")
  expect_equal(mode_all(x13, na.rm = TRUE), 1)
  expect_equal(mode_all(x14, na.rm = TRUE), c(1, 2))
  expect_equal(mode_all(x15, na.rm = TRUE), 1)
  expect_equal(mode_all(x16, na.rm = TRUE), 1)
  expect_equal(mode_all(x17, na.rm = TRUE), c(1, 2))
})


# 3. `mode_single()`

test_that("`mode_single()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_single(x3) , 7)
  expect_equal(mode_single(x10), 1)
})

test_that("`mode_single()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_single(x2) , NA_real_)
  expect_equal(mode_single(x5) , NA_real_)
  expect_equal(mode_single(x8) , NA_real_)
  expect_equal(mode_single(x9) , NA_real_)
  expect_equal(mode_single(x11), NA_real_)
  expect_equal(mode_single(x12), NA_character_)
  expect_equal(mode_single(x13), NA_real_)
  expect_equal(mode_single(x14), NA_real_)
  expect_equal(mode_single(x15), NA_real_)
  expect_equal(mode_single(x16), NA_real_)
  expect_equal(mode_single(x17), NA_real_)
})

test_that("`mode_single()` is right with some `NA` input and `na.rm = TRUE`
          (but still `NA` output)", {
  expect_equal(mode_single(x2 , na.rm = TRUE), 2)
  expect_equal(mode_single(x5 , na.rm = TRUE), NA_real_)
  expect_equal(mode_single(x8 , na.rm = TRUE), 1)
  expect_equal(mode_single(x9 , na.rm = TRUE), 1)
  expect_equal(mode_single(x11, na.rm = TRUE), 1)
  expect_equal(mode_single(x12, na.rm = TRUE), "a")
  expect_equal(mode_single(x13, na.rm = TRUE), 1)
  expect_equal(mode_single(x14, na.rm = TRUE), NA_real_)
  expect_equal(mode_single(x15, na.rm = TRUE), 1)
  expect_equal(mode_single(x16, na.rm = TRUE), 1)
  expect_equal(mode_single(x17, na.rm = TRUE), NA_real_)
})


# 4. `mode_count()`

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


# 5. `mode_frequency()`

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



# Possible modes ----------------------------------------------------------

# 1. `mode_possible_min()`

test_that("`mode_possible_min()` works correctly", {
  expect_equal(mode_possible_min(x1 ), 9)
  expect_equal(mode_possible_min(x2 ), NA_real_)
  expect_equal(mode_possible_min(x3 ), 7)
  expect_equal(mode_possible_min(x4 ), c("a", "b"))
  expect_equal(mode_possible_min(x5 ), NA_real_)
  expect_equal(mode_possible_min(x6 ), 5)
  expect_equal(mode_possible_min(x7 ), c("y", "z"))
  expect_equal(mode_possible_min(x8 ), 1)
  expect_equal(mode_possible_min(x9 ), 1)
  expect_equal(mode_possible_min(x10), 1)
  expect_equal(mode_possible_min(x11), 1)
  expect_equal(mode_possible_min(x12), "a")
  expect_equal(mode_possible_min(x13), 1)
  expect_equal(mode_possible_min(x14), NA_real_)
  expect_equal(mode_possible_min(x15), NA_real_)
  expect_equal(mode_possible_min(x16), 1)
  expect_equal(mode_possible_min(x17), NA_real_)
})


# 2. `mode_possible_max()`

test_that("`mode_possible_max()` works correctly by default", {
  expect_equal(mode_possible_max(x1 ), 9)
  expect_equal(mode_possible_max(x2 ), c(2, 1))
  expect_equal(mode_possible_max(x3 ), 7)
  expect_equal(mode_possible_max(x4 ), c("a", "b"))
  expect_equal(mode_possible_max(x5 ), NA_real_)
  expect_equal(mode_possible_max(x6 ), 5)
  expect_equal(mode_possible_max(x7 ), c("y", "z"))
  expect_equal(mode_possible_max(x8 ), c(1, 2))
  expect_equal(mode_possible_max(x9 ), c(1, 2))
  expect_equal(mode_possible_max(x10), 1)
  expect_equal(mode_possible_max(x11), 1)
  expect_equal(mode_possible_max(x12), c("a", "b"))
  expect_equal(mode_possible_max(x13), 1)
  expect_equal(mode_possible_max(x14), NA_real_)
  expect_equal(mode_possible_max(x15), 1)
  expect_equal(mode_possible_max(x16), NA_real_)
  expect_equal(mode_possible_max(c(x16, 1)), 1)
  expect_equal(mode_possible_max(x17), NA_real_)
})

test_that("`mode_possible_max()` works correctly with `multiple = TRUE`", {
  expect_equal(mode_possible_max(x1 , TRUE), 9)
  expect_equal(mode_possible_max(x2 , TRUE), c(2, 1))
  expect_equal(mode_possible_max(x3 , TRUE), 7)
  expect_equal(mode_possible_max(x4 , TRUE), c("a", "b"))
  expect_equal(mode_possible_max(x5 , TRUE), c(1, 2))
  expect_equal(mode_possible_max(x6 , TRUE), c(5))
  expect_equal(mode_possible_max(x7 , TRUE), c("y", "z"))
  expect_equal(mode_possible_max(x8 , TRUE), c(1, 2))
  expect_equal(mode_possible_max(x9 , TRUE), c(1, 2))
  expect_equal(mode_possible_max(x10, TRUE), 1)
  expect_equal(mode_possible_max(x11, TRUE), c(1))
  expect_equal(mode_possible_max(x12, TRUE), c("a", "b"))
  expect_equal(mode_possible_max(x13, TRUE), c(1))
  expect_equal(mode_possible_max(x14, TRUE), c(1, 2, 3))
  expect_equal(mode_possible_max(x15, TRUE), c(1))
  expect_equal(mode_possible_max(x16, TRUE), c(1, 2, 3))
  expect_equal(mode_possible_max(x17, TRUE), c(1, 2))
})

# 3. `mode_count_range()`

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

test_that("`mode_count_range()` works correctly with `exclusive = TRUE`", {
  expect_equal(mode_count_range(x1 , TRUE), c(1L, 1L))
  expect_equal(mode_count_range(x2 , TRUE), c(1L, 2L))
  expect_equal(mode_count_range(x3 , TRUE), c(1L, 1L))
  expect_equal(mode_count_range(x4 , TRUE), c(2L, 2L))
  # `x5` has a different expectation here: Since the `NA` is known to be either
  # `1` or `2`, it must tip the balance either way, leading to a single mode!
  expect_equal(mode_count_range(x5 , TRUE), c(1L, 1L))
  expect_equal(mode_count_range(x6 , TRUE), c(1L, 1L))
  expect_equal(mode_count_range(x7 , TRUE), c(2L, 2L))
  expect_equal(mode_count_range(x8 , TRUE), c(1L, 2L))
  expect_equal(mode_count_range(x9 , TRUE), c(1L, 2L))
  expect_equal(mode_count_range(x10, TRUE), c(1L, 1L))
  # `x11` has only one known value, so if it's exclusive, the `NA` must
  # represent that value, as well:
  expect_equal(mode_count_range(x11, TRUE), c(1L, 1L))
  expect_equal(mode_count_range(x12, TRUE), c(1L, 2L))
  # (See `x11`.)
  expect_equal(mode_count_range(x13, TRUE), c(1L, 1L))
  expect_equal(mode_count_range(x14, TRUE), c(1L, 3L))
  # (See `x11`.)
  expect_equal(mode_count_range(x15, TRUE), c(1L, 1L))
  expect_equal(mode_count_range(x16, TRUE), c(1L, 2L))
  # As in `x11`, except there are two known values, so the `NA`s must be
  # fully distributed across them instead of forming a new mode:
  expect_equal(mode_count_range(x17, TRUE), c(1L, 2L))
})

# 4. `mode_frequency_range()`

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

# 5. `mode_is_trivial()`

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
})



# Helpers -----------------------------------------------------------------

test_that("`warn_if_factor_not_exclusive()` works correctly", {
  x12_factor <- as.factor(x12)
  x17_factor <- as.factor(x17)
  # Also check that the warning includes the extra part that is conditionally
  # pasted onto the basic part:
  expect_warning(mode_count_range(x12_factor, FALSE), "this particular case")
  # Also check that the warning does *not* include the extra part; i.e., it ends
  # on the basic part:
  expect_warning(mode_count_range(x17_factor, FALSE), "are known).$")
  # No warning should be thrown if `exclusive` was set to `TRUE`:
  expect_no_warning(mode_count_range(x12_factor, TRUE))
  expect_no_warning(mode_count_range(x17_factor, TRUE))
})

