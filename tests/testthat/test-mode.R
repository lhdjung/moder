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
  expect_equal(mode_single(x4), NA)
  expect_equal(mode_single(x6), 5)
  expect_equal(mode_single(x7), NA)
})

test_that("`mode_count()` is right when no `NA`s are present", {
  expect_equal(mode_count(x1), 1)
  expect_equal(mode_count(x4), 2)
  expect_equal(mode_count(x6), 1)
  expect_equal(mode_count(x7), 2)
})



# Actual modes, with `NA`s ------------------------------------------------

# 1. `mode_first()`

test_that("`mode_first()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_first(x3) , 7)
  expect_equal(mode_first(x8) , 1)
  expect_equal(mode_first(x9) , 1)
  expect_equal(mode_first(x10), 1)
  expect_equal(mode_first(x11), 1)
  expect_equal(mode_first(x12), "a")
  expect_equal(mode_first(x13), 1)
})

test_that("`mode_first()` is right with some `NA` input, non-`NA` output,
          and `first_known = FALSE`", {
  expect_equal(mode_first(x3 , first_known = FALSE), 7)
  expect_equal(mode_first(x8 , first_known = FALSE), 1)
  expect_equal(mode_first(x10, first_known = FALSE), 1)
  expect_equal(mode_first(x11, first_known = FALSE), 1)
  expect_equal(mode_first(x12, first_known = FALSE), "a")
  expect_equal(mode_first(x13, first_known = FALSE), NA_real_)
})

test_that("`mode_first()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_first(x2 ), NA_real_)
  expect_equal(mode_first(x5 ), NA_real_)
  expect_equal(mode_first(x14), NA_real_)
})

test_that("`mode_first()` is right with some `NA` input and `na.rm = TRUE`", {
  expect_equal(mode_first(x2 , na.rm = TRUE), 2)
  expect_equal(mode_first(x5 , na.rm = TRUE), 1)
  expect_equal(mode_first(x14, na.rm = TRUE), 1)
})

test_that("`mode_first()` is right with some `NA` input, `NA` output,
          and `first_known = FALSE`", {
  expect_equal(mode_first(x2 , first_known = FALSE), NA_real_)
  expect_equal(mode_first(x5 , first_known = FALSE), NA_real_)
  expect_equal(mode_first(x9 , first_known = FALSE), NA_real_)
  expect_equal(mode_first(x13, first_known = FALSE), NA_real_)
  expect_equal(mode_first(x14, first_known = FALSE), NA_real_)
})

test_that("`mode_first()` is right with some `NA` input, `na.rm = TRUE`,
          and `first_known = FALSE`", {
  expect_equal(mode_first(x2 , na.rm = TRUE, first_known = FALSE), 2)
  expect_equal(mode_first(x5 , na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x9 , na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x13, na.rm = TRUE, first_known = FALSE), 1)
  expect_equal(mode_first(x14, na.rm = TRUE, first_known = FALSE), 1)
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
})

test_that("`mode_all()` is right with some `NA` input and `na.rm = TRUE`", {
  expect_equal(mode_all(x2 , na.rm = TRUE), 2)
  expect_equal(mode_all(x5 , na.rm = TRUE), c(1, 2))
  expect_equal(mode_all(x8 , na.rm = TRUE), 1)
  expect_equal(mode_all(x9 , na.rm = TRUE), 1)
  expect_equal(mode_all(x11, na.rm = TRUE), 1)
  expect_equal(mode_all(x12, na.rm = TRUE), "a")
  expect_equal(mode_all(x13, na.rm = TRUE), 1)
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
})

test_that("`mode_single()` is right with some `NA` input and `na.rm = TRUE`
          (but still `NA` output)", {
  expect_equal(mode_single(x2) , NA_real_)
  expect_equal(mode_single(x5) , NA_real_)
  expect_equal(mode_single(x8) , NA_real_)
  expect_equal(mode_single(x9) , NA_real_)
  expect_equal(mode_single(x11), NA_real_)
  expect_equal(mode_single(x12), NA_character_)
  expect_equal(mode_single(x13), NA_real_)
})


# 4. `mode_count()`

test_that("`mode_count()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_count(x3) , 1)
  expect_equal(mode_count(x10), 1)
})

test_that("`mode_count()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_count(x2) , NA_integer_)
  expect_equal(mode_count(x5) , NA_integer_)
  expect_equal(mode_count(x8) , NA_integer_)
  expect_equal(mode_count(x9) , NA_integer_)
  expect_equal(mode_count(x11), NA_integer_)
  expect_equal(mode_count(x12), NA_integer_)
  expect_equal(mode_count(x13), NA_integer_)
})

test_that("`mode_count()` is right with some `NA` input and `na.rm = TRUE`", {
  expect_equal(mode_count(x2 , na.rm = TRUE), 1)
  expect_equal(mode_count(x5 , na.rm = TRUE), 2)
  expect_equal(mode_count(x8 , na.rm = TRUE), 1)
  expect_equal(mode_count(x9 , na.rm = TRUE), 1)
  expect_equal(mode_count(x11, na.rm = TRUE), 1)
  expect_equal(mode_count(x12, na.rm = TRUE), 1)
  expect_equal(mode_count(x13, na.rm = TRUE), 1)
})



# Possible modes ----------------------------------------------------------

# 1. `mode_possible_min()`

test_that("`mode_possible_min()` works correctly", {
  expect_equal(mode_possible_min(x1 ), 9)
  expect_equal(mode_possible_min(x2 ), 2)
  expect_equal(mode_possible_min(x3 ), 7)
  expect_equal(mode_possible_min(x4 ), c("a", "b"))
  expect_equal(mode_possible_min(x5 ), c(1, 2))
  expect_equal(mode_possible_min(x6 ), 5)
  expect_equal(mode_possible_min(x7 ), c("y", "z"))
  expect_equal(mode_possible_min(x8 ), 1)
  expect_equal(mode_possible_min(x9 ), 1)
  expect_equal(mode_possible_min(x10), 1)
  expect_equal(mode_possible_min(x11), 1)
  expect_equal(mode_possible_min(x12), "a")
  expect_equal(mode_possible_min(x13), 1)
  expect_equal(mode_possible_min(x5, accept_known = FALSE), NA_real_)
})


# 2. `mode_possible_max()`

test_that("`mode_possible_max()` works correctly", {
  expect_equal(mode_possible_max(x1 ), 9)
  expect_equal(mode_possible_max(x2 ), c(2, 1))
  expect_equal(mode_possible_max(x3 ), 7)
  expect_equal(mode_possible_max(x4 ), c("a", "b"))
  expect_equal(mode_possible_max(x5 ), c(1, 2))
  expect_equal(mode_possible_max(x6 ), 5)
  expect_equal(mode_possible_max(x7 ), c("y", "z"))
  expect_equal(mode_possible_max(x8 ), c(1, 2))
  expect_equal(mode_possible_max(x9 ), c(1, 2))
  expect_equal(mode_possible_max(x10), 1)
  expect_equal(mode_possible_max(x11), 1)
  expect_equal(mode_possible_max(x12), c("a", "b"))
  expect_equal(mode_possible_max(x13), 1)
})

# 3. `mode_count_possible_min()`

test_that("`mode_count_possible_min()` works correctly", {
  expect_equal(mode_count_possible_min(x1 ), 1L)
  expect_equal(mode_count_possible_min(x2 ), 1L)
  expect_equal(mode_count_possible_min(x3 ), 1L)
  expect_equal(mode_count_possible_min(x4 ), 2L)
  expect_equal(mode_count_possible_min(x5 ), 2L)
  expect_equal(mode_count_possible_min(x6 ), 1L)
  expect_equal(mode_count_possible_min(x7 ), 2L)
  expect_equal(mode_count_possible_min(x8 ), 1L)
  expect_equal(mode_count_possible_min(x9 ), 1L)
  expect_equal(mode_count_possible_min(x10), 1L)
  expect_equal(mode_count_possible_min(x11), 1L)
  expect_equal(mode_count_possible_min(x12), 1L)
  expect_equal(mode_count_possible_min(x13), 1L)
  expect_equal(mode_count_possible_min(x5, accept_known = FALSE), NA_real_)
})

# 4. `mode_count_possible_max()`

test_that("`mode_count_possible_max()` works correctly", {
  expect_equal(mode_count_possible_max(x1 ), 1L)
  expect_equal(mode_count_possible_max(x2 ), 2L)
  expect_equal(mode_count_possible_max(x3 ), 1L)
  expect_equal(mode_count_possible_max(x4 ), 2L)
  expect_equal(mode_count_possible_max(x5 ), 2L)
  expect_equal(mode_count_possible_max(x6 ), 1L)
  expect_equal(mode_count_possible_max(x7 ), 2L)
  expect_equal(mode_count_possible_max(x8 ), 2L)
  expect_equal(mode_count_possible_max(x9 ), 2L)
  expect_equal(mode_count_possible_max(x10), 1L)
  expect_equal(mode_count_possible_max(x11), 1L)
  expect_equal(mode_count_possible_max(x12), 2L)
  expect_equal(mode_count_possible_max(x13), 1L)
})

