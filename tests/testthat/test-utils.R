
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

