
# Test vectors:
list_length <- 50L
list1 <- vector("list", list_length)
modes1_all <- vector("list", list_length)
modes2_all <- modes1_all
modes1_first <- integer(list_length)
modes2_first <- modes1_first

for (i in 1L:list_length) {
  list1[[i]] <- trunc(rnorm(10L, 50L, 5L))
}

for (i in 1L:list_length) {
  modes1_all[[i]]   <- mode_all(list1[[i]])
  modes2_all[[i]]   <- mode_all_if_no_na(list1[[i]])
  modes1_first[[i]] <- mode_first(list1[[i]])
  modes2_first[[i]] <- mode_first_if_no_na(list1[[i]])
}

test_that("`mode_all_if_no_na()` works correctly", {
  expect_equal(modes1_all, modes2_all)
})

test_that("`mode_first_if_no_na()` works correctly", {
  expect_equal(modes1_first, modes2_first)
})


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

