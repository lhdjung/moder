
# Without `NA`s -----------------------------------------------------------

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



# With `NA`s --------------------------------------------------------------

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
          and `accept = FALSE`", {
  expect_equal(mode_first(x3 , accept = FALSE), 7)
  expect_equal(mode_first(x8 , accept = FALSE), 1)
  expect_equal(mode_first(x10, accept = FALSE), 1)
  expect_equal(mode_first(x11, accept = FALSE), 1)
  expect_equal(mode_first(x12, accept = FALSE), "a")
  expect_equal(mode_first(x13, accept = FALSE), NA_real_)
  expect_equal(mode_first(x16, accept = FALSE), 1)
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
          and `accept = FALSE`", {
  expect_equal(mode_first(x2 , accept = FALSE), NA_real_)
  expect_equal(mode_first(x5 , accept = FALSE), NA_real_)
  expect_equal(mode_first(x9 , accept = FALSE), NA_real_)
  expect_equal(mode_first(x13, accept = FALSE), NA_real_)
  expect_equal(mode_first(x14, accept = FALSE), NA_real_)
  expect_equal(mode_first(x15, accept = FALSE), NA_real_)
  expect_equal(mode_first(x15, accept = FALSE), NA_real_)
})

test_that("`mode_first()` is right with some `NA` input, `na.rm = TRUE`,
          and `accept = FALSE`", {
  expect_equal(mode_first(x2 , na.rm = TRUE, accept = FALSE), 2)
  expect_equal(mode_first(x5 , na.rm = TRUE, accept = FALSE), 1)
  expect_equal(mode_first(x9 , na.rm = TRUE, accept = FALSE), 1)
  expect_equal(mode_first(x13, na.rm = TRUE, accept = FALSE), 1)
  expect_equal(mode_first(x14, na.rm = TRUE, accept = FALSE), 1)
  expect_equal(mode_first(x15, na.rm = TRUE, accept = FALSE), 1)
  expect_equal(mode_first(x16, na.rm = TRUE, accept = FALSE), 1)
  expect_equal(mode_first(x17, na.rm = TRUE, accept = FALSE), 1)
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

test_that("`mode_single()` is right with some `NA` input and `accept = TRUE`", {
  expect_equal(mode_single(x2 , accept = TRUE), NA_real_)
  # # Weird crossover with `x5` from missings.Rmd can occur here:
  expect_equal(mode_single(x5 , accept = TRUE), NA_real_)
  expect_equal(mode_single(x8 , accept = TRUE), 1)
  expect_equal(mode_single(x9 , accept = TRUE), 1)
  expect_equal(mode_single(x11, accept = TRUE), 1)
  expect_equal(mode_single(x12, accept = TRUE), "a")
  expect_equal(mode_single(x13, accept = TRUE), 1)
  expect_equal(mode_single(x14, accept = TRUE), NA_real_)
  expect_equal(mode_single(x15, accept = TRUE), NA_real_)
  expect_equal(mode_single(x16, accept = TRUE), 1)
  expect_equal(mode_single(x17, accept = TRUE), NA_real_)
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

test_that("`mode_single()` throws an error if `multiple` has length > 1", {
  expect_error(mode_single(x3, multiple = 1:2))
  expect_error(mode_single(x4, multiple = 1:2))
})

test_that("`na.rm.amount` is right with `x2`", {
  expect_equal(mode_first(x2, na.rm.amount = 0), NA_real_)
  expect_equal(mode_first(x2, na.rm.amount = 1), NA_real_)
  expect_equal(mode_first(x2, na.rm.amount = 2), NA_real_)
  expect_equal(mode_first(x2, na.rm.amount = 3), 2)
})

test_that("`na.rm.amount` is right with `x3`", {
  expect_equal(mode_first(x3, na.rm.amount = 0), 7)
  expect_equal(mode_first(x3, na.rm.amount = 1), 7)
})

test_that("`na.rm.amount` is right with `x5`", {
  expect_equal(mode_first(x5, na.rm.amount = 0), NA_real_)
  expect_equal(mode_first(x5, na.rm.amount = 1), 1)
})

test_that("`na.rm.amount` is right with `x9`", {
  expect_equal(mode_first(x9, na.rm.amount = 0), NA_real_)
  expect_equal(mode_first(x9, na.rm.amount = 1), 1)
})
