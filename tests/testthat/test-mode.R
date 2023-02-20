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


# Non-`NA` distributions --------------------------------------------------

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


# Distributions with `NA`s ------------------------------------------------

test_that("`mode_first()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_first(x3), 7)
  expect_equal(mode_first(x8), 1)
  expect_equal(mode_first(x9), 1)
})

test_that("`mode_first()` is right with some `NA` input but non-`NA` output,
          and with `first_known = FALSE`", {
  expect_equal(mode_first(x3, first_known = FALSE), 7)
  expect_equal(mode_first(x8, first_known = FALSE), 1)
  expect_equal(mode_first(x9, first_known = FALSE), NA_real_)
})

test_that("`mode_first()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_first(x2), NA_real_)
  expect_equal(mode_first(x5), NA_real_)
  expect_equal(mode_first(x10), 1)
  expect_equal(mode_first(x11), 1)
})

test_that("`mode_first()` is right with some `NA` input but non-`NA` output,
          and with `first_known = FALSE`", {
  expect_equal(mode_first(x2, first_known = FALSE), NA_real_)
  expect_equal(mode_first(x5, first_known = FALSE), NA_real_)
  expect_equal(mode_first(x10, first_known = FALSE), 1)
  expect_equal(mode_first(x11, first_known = FALSE), 1)
})


test_that("`mode_all()` is right with some `NA` input but non-`NA` output", {
  expect_equal(mode_all(x3), 7)
})

test_that("`mode_all()` is right with some `NA` input and `NA` output", {
  expect_equal(mode_all(x8), NA_real_)
  expect_equal(mode_all(x9), NA_real_)
  expect_equal(mode_all(x12), NA_character_)
})



