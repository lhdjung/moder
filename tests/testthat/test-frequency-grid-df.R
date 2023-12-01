
# Test vectors:
x1 <- c("a", "a", "a", "b", "b", "c", rep(NA, times = 5))
x2 <- c(1, 1, 2, 3, rep(NA, times = 6))


test_that("`frequency_grid_df()` works with `x1`", {
  expect_equal(frequency_grid_df(x1), structure(list(
    x = c("a", "a", "a", "a", "b", "b", "b", "b", "c", "c", "c", "c"),
    freq = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L),
    is_missing = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    can_be_filled = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
    is_supermodal = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  ), class = "data.frame", row.names = c(NA, -12L)))
})

test_that("`frequency_grid_df()` works with `x2`", {
  expect_equal(frequency_grid_df(x2), structure(list(
    x = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    freq = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L),
    is_missing = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    can_be_filled = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE),
    is_supermodal = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
  ), class = "data.frame", row.names = c(NA, -12L)))
})

