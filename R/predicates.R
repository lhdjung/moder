#' Is a vector unimodal or multimodal?
#'
#' @description Two complementary functions:
#'
#' - `is_unimodal()` checks whether a vector has exactly one mode.
#' - `is_multimodal()` checks whether a vector has multiple modes.
#'
#' @param x A vector to search for its modes.
#' @inheritParams mode_count
#'
#' @details These functions are thin wrappers around [`mode_count()`], a more
#'   general tool. See examples there for `NA` handling.
#'
#' @inheritSection is_uniform Factors and `max_unique`
#'
#' @return Logical (length 1).
#'
#' @name is-unimodal-multimodal
#'
#' @export
#'
#' @seealso [`is_uniform()`] to check for a uniform distribution.
#'
#' @examples
#' # Only one mode:
#' is_unimodal(c(5, 5, 6))
#' is_multimodal(c(5, 5, 6))
#'
#' # Two modes:
#' is_unimodal(c(5, 5, 6, 6))
#' is_multimodal(c(5, 5, 6, 6))
#'
#' # Unclear because of the missing value:
#' is_unimodal(c(5, 5, 6, NA))
#' is_multimodal(c(5, 5, 6, NA))

is_unimodal <- function(x, na.rm = FALSE, max_unique = NULL) {
  mode_count(x, na.rm = na.rm, max_unique = max_unique) == 1L
}

#' @rdname is-unimodal-multimodal
#' @export
is_multimodal <- function(x, na.rm = FALSE, max_unique = NULL) {
  mode_count(x, na.rm = na.rm, max_unique = max_unique) > 1L
}


#' Are the values uniformly distributed?
#'
#' @description `is_uniform()` checks whether all values in a given vector are
#'   equally frequent, i.e., uniformly distributed. The mode is trivial in such
#'   cases.
#'
#' @inheritParams mode_all
#' @param max_unique Numeric or string. If the maximum number of unique values
#'   in `x` is known, set `max_unique` to that number. This puts a limit on how
#'   many unique values may be represented by `NA`s. Set it to `"known"` instead
#'   if no values beyond those already known can occur in `x`, or if `x` is a
#'   factor (it will then count the levels). Default is `NULL`, which assumes no
#'   maximum.
#'
#' @details The function returns `TRUE` if `x` has length < 3 because no value
#'   is more frequent than another one. Otherwise, it returns `NA` in these
#'   cases:
#'   - Some `x` values are missing, all known values are equal, and `max_unique`
#'   is not `1` or `"known"`. Thus, it is unknown whether there are values with
#'   different frequencies.
#'   - All known values are modes if the `NA`s "fill up" all non-modal known
#'   values exactly, i.e., without any `NA`s remaining. These values might then
#'   hypothetically be modes.
#'   - There are so many missing values that they might form mode-sized groups
#'   of values that are not among the known values, and the number of `NA`s is
#'   divisible by the modal frequency so that all (partly hypothetical) values
#'   might be equally frequent. You can limit the number of such hypothetical
#'   values by specifying `max_unique`. The function might then return `FALSE`
#'   instead of `NA`.
#'   - Some `NA`s remain after "filling up" the non-modal known values with
#'   `NA`s and / or hypothetically forming new mode-sized groups, and the number
#'   of remaining `NA`s is divisible by the number of unique known values. Thus,
#'   these surplus `NA`s can be evenly distributed across the known (or,
#'   perhaps, partly hypothesized) values. Thus, a uniform distribution is still
#'   possible. `max_unique` can be relevant here, as well.
#'
#' @return Logical (length 1).
#'
#' @export
#'
#' @section Factors and `max_unique`: If `x` is a factor, `max_unique` should be
#'   `"known"` or there is a warning. This is because a factor's levels are
#'   supposed to include all of its possible values.
#'
#' @examples
#' # The distribution is uniform
#' # if all values are equal...
#' is_uniform(c(1, 1, 1))
#'
#' # ...and even if there are multiple unique
#' # values but they are all equally frequent:
#' is_uniform(c(1, 1, 2, 2))
#'
#' # This includes cases where
#' # all values are different:
#' is_uniform(c(1, 2, 3))
#'
#' # Here, the mode is nontrivial
#' # because `1` is more frequent than `2`:
#' is_uniform(c(1, 1, 2))
#'
#' # Two of the `NA`s might be `8`s, and
#' # the other three might represent a value
#' # different from both `7` and `8`. Thus,
#' # it's possible that all three distinct
#' # values are equally frequent:
#' is_uniform(c(7, 7, 7, 8, rep(NA, 5)))
#'
#' # The same is not true if all values,
#' # even the missing ones, must represent
#' # one of the known values:
#' is_uniform(c(7, 7, 7, 8, rep(NA, 5)), max_unique = "known")

is_uniform <- function(x, na.rm = FALSE, max_unique = NULL) {
  n_x <- length(x)
  x <- x[!is.na(x)]
  if (na.rm) {
    n_x <- length(x)
    n_na <- 0L
  } else {
    n_na <- n_x - length(x)
  }
  unique_x <- unique(x)
  max_unique <- handle_max_unique_input(
    x, max_unique, length(unique_x), n_na, "is_uniform"
  )
  # Some edge cases to check for:
  # -- With less than three values, none can be more frequent than another.
  # -- In the next most simple case, all values are known, and it is trivial to
  # determine whether all of them are equally frequent.
  if (n_x < 3L) {
    return(TRUE)
  } else if (n_na == 0L) {
    frequency_known <- table(x)
    uniformity_is_true <- all(frequency_known == frequency_known[1L])
    return(uniformity_is_true)
  }
  # Special rules apply in case all known values are equal:
  # -- If the user specified `max_unique` such that no unique values beyond the
  # single known one can exist, all values including the `NA`s must be equal.
  # -- Otherwise, there may or may not be other values with different
  # frequencies than the known one. Due to this uncertainty, `NA` is returned.
  if (length(unique_x) == 1L) {
    if (!is.null(max_unique) && max_unique == 1) {
      return(TRUE)
    } else {
      return(NA)
    }
  }
  # Determine the set of modes among known values using an internal helper:
  modes <- mode_all_if_no_na(x)
  # Special rules also apply if all known values are equally frequent:
  # -- If both the number of missing values and the modal frequency are exactly
  # one, the one missing value may or may not represent one of the known values,
  # barring `max_unique` specifications that rule this out. This scenario can't
  # be affirmed or ruled out, so `NA` is returned.
  # -- Uniformity is also unknown if the number of `NA`s can be divided by the
  # number of unique values (which is the number of modes here). The `NA`s might
  # then be evenly distributed across the unique values. Equivalently, sets of
  # `NA`s may remain, each of the size of the modal frequency, and form entirely
  # different values, i.e., different from the known values in `x`. This, too,
  # might be ruled out by `max_unique`.
  # -- If neither of this is the case, the `NA`s must belong to values that are
  # more or less frequent than others, so `FALSE` is returned.
  if (all(unique_x %in% modes)) {
    uniformity_is_unknown_single_na <- n_na == 1L &&
      length(x) == length(unique_x) &&
      !isTRUE(max_unique == length(unique_x))
    if (uniformity_is_unknown_single_na) {
      return(NA)
    }
    uniformity_is_unknown_modulo <- n_na %% length(modes) == 0L || (
      !isTRUE(max_unique == length(unique_x)) &&
        n_na %% length(x[x == modes[[1L]]]) == 0L
    )
    if (uniformity_is_unknown_modulo) {
      return(NA)
    }
    return(FALSE)
  }
  # Reduce `n_na` to the number of `NA`s that are left after distributing other
  # `NA`s among the non-modal values such that they become modes in a
  # hypothetical scenario:
  n_slots_empty <- count_slots_empty(x)
  n_na_surplus <- n_na - n_slots_empty
  frequency_max <- length(x[x %in% modes[[1L]]])
  # Some more special rules, this time concerning user-imposed restrictions on
  # the number of unique values in `x`:
  if (!is.null(max_unique)) {
    n_slots_known_vals <- frequency_max * length(unique_x)
    n_slots_new_vals <- frequency_max * (max_unique - length(unique_x))
    if (n_slots_new_vals == 0L) {
      if (n_na_surplus %% length(unique_x) == 0L) {
        return(NA)
      } else {
        return(FALSE)
      }
    }
    n_slots_all <- n_slots_known_vals + n_slots_new_vals
    n_na_super_surplus <- n_na_surplus - n_slots_new_vals
    uniformity_is_unknown <- n_na_super_surplus == 0L ||
      (n_na_super_surplus > 1L && n_slots_all %% n_na_super_surplus == 0L) ||
      (n_na_surplus > 1L && n_slots_known_vals %% n_na_surplus == 0L)
    if (uniformity_is_unknown) {
      return(NA)
    } else {
      return(FALSE)
    }
  }
  # -- If the number of missing values is zero or positive and it can be divided
  # by the number of unique values, these `NA`s might fill up the empty slots,
  # so there is a possibility that all values are equally frequent. The same is
  # true if the count of remaining `NA`s can be divided by the modal frequency
  # because they might represent values beyond the known ones; see above.
  # -- Otherwise, there are some `NA`s that cannot be part of a group with the
  # modal frequency. They must be values with a lesser frequency, so there are
  # different frequencies among `x` values.
  uniformity_is_unknown <- n_na_surplus >= 0L &&
    any(n_na_surplus %% c(length(unique_x), frequency_max) == 0L)
  if (uniformity_is_unknown) {
    NA
  } else {
    FALSE
  }
}


#' Is the mode trivial?
#'
#' @description `mode_is_trivial()` was renamed to [`is_uniform()`]. Please use
#'   this new function instead.
#'
#'   The function with the old name is deprecated and will be removed in a
#'   future version. It is retained for now, but using it will cause a warning.
#'
#' @inheritParams is_uniform
#'
#' @return Logical (length 1).
#'
#' @export

mode_is_trivial <- function(x, na.rm = FALSE, max_unique = NULL) {
  warning(paste(
    "`mode_is_trivial()` was renamed to `is_uniform()` in moder 0.4.0.",
    "The function with the old name will be removed in a future version."
  ))
  is_uniform(x = x, na.rm = na.rm, max_unique = max_unique)
}

