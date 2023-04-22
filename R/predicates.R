#' Is the mode trivial?
#'
#' @description `mode_is_trivial()` checks whether all values in a given vector
#'   are equally frequent. The mode is not too informative in such cases.
#'
#' @inheritParams mode_all
#' @param exclusive Boolean. Can `NA`s only represent known values? Set
#'   `exclusive` to `TRUE` if you are certain that there are no other values
#'   behind the `NA`s, such that each `NA` masks one of the known values.
#'   Default is `FALSE`.
#'
#' @details The function returns `TRUE` whenever `x` has length < 3 because no
#'   value is more frequent than another one. Otherwise, it returns `NA` in
#'   these cases:
#'   - Some `x` values are missing and all known values are equal. Thus, it is
#'   unknown whether there is a value with a different frequency.
#'   - All known values are modes if the `NA`s "fill up" the non-modal values
#'   exactly, i.e., without any `NA`s remaining.
#'   - There are so many missing values that they might form mode-sized groups
#'   of values that are not among the known values, and the number of missing
#'   values is divisible by the number of unique known values (even after
#'   "filling up" the non-modal values with `NA`s so that they are
#'   hypothetically modes). Suppress this behavior by setting `exclusive` to
#'   `TRUE` if you are certain that there are no values in `x` other than those
#'   already known to occur. The function will then return `FALSE` instead of
#'   `NA`.
#'
#' @return Boolean (length 1).
#'
#' @export
#'
#' @examples
#' # The mode is trivial if
#' # all values are equal...
#' mode_is_trivial(c(1, 1, 1))
#'
#' # ...and even if all unique
#' # values are equally frequent:
#' mode_is_trivial(c(1, 1, 2, 2))
#'
#' # It's also trivial if
#' # all values are different:
#' mode_is_trivial(c(1, 2, 3))
#'
#' # Here, the mode is nontrivial
#' # because `1` is more frequent than `2`:
#' mode_is_trivial(c(1, 1, 2))

mode_is_trivial <- function(x, na.rm = FALSE, max_unique = NULL) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # There are some edge cases to check for. With less than three values, none
  # can be more frequent than the other:
  if (length(x) < 3L) {
    return(TRUE)
  }
  n_x <- length(x)
  x <- x[!is.na(x)]
  n_na <- n_x - length(x)
  rm(n_x)
  unique_x <- unique(x)
  # This chunk should be part of each function that has a `max_unique` argument:
  if (is.null(max_unique)) {
    check_factor_max_unique(x, n_na, "mode_is_trivial")
  } else {
    max_unique <- handle_max_unique_input(
      x, max_unique, length(unique_x), n_na, "mode_is_trivial"
    )
  }
  # If some values are missing (from a length >= 3 vector) and all known values
  # are equal, it's unknown whether there is a value with a different frequency:
  if (n_na > 0L && length(unique_x) == 1L) {
    return(NA)
  }
  modes <- mode_all_if_no_na(x)
  # Special rules apply if all known values are equally frequent:
  # -- If there are no `NA`s, there are no values beyond the known ones, so
  # `TRUE` is returned.
  # -- If the number of `NA`s can be divided by the number of unique values
  # (here, the number of modes), sets of `NA`s may remain, each of the size of
  # the modal frequency, and form entirely different values, i.e., different
  # from the known values in `x`. This latter scenario is far from certain, but
  # it cannot be ruled out (unless the user does so by setting `exclusive =
  # TRUE`), so `NA` is returned.
  # -- If this is not case but there are still `NA`s, at least some of these
  # must belong to less frequent values, so `FALSE` is returned.
  if (all(unique_x %in% modes)) {
    if (n_na == 0L) {
      return(TRUE)
    } else if (n_na %% length(modes) == 0L) {
      return(NA)
    } else {
      return(FALSE)
    }
  }
  # Reduce `n_na` to the number of `NA`s that are left after distributing other
  # `NA`s among the non-modal values such that they become modes in a
  # hypothetical scenario:
  n_slots_empty <- count_slots_empty(x)
  n_na_surplus <- n_na - n_slots_empty

  # Some more special rules, this time concerning user-imposed restrictions on
  # the number of unique values in `x`:
  if (!is.null(max_unique) && n_na > 0L) {
    frequency_max <- length(x[x %in% modes[[1L]]])
    n_slots_known_vals <- frequency_max * length(unique_x)
    n_slots_new_vals <- frequency_max * (max_unique - length(unique_x))
    n_slots_all <- n_slots_known_vals + n_slots_new_vals
    n_na_super_surplus <- n_na_surplus - n_slots_new_vals
    if (
      n_na_super_surplus == 0L ||
      (n_na_super_surplus > 1L && n_slots_all %% n_na_super_surplus == 0L) ||
      (n_na_surplus > 1L && n_slots_known_vals %% n_na_surplus == 0L)
    ) {
      return(NA)
    } else {
      return(FALSE)
    }

    if (n_slots_new_vals < n_na_surplus) {
      return((n_na_surplus - n_slots_new_vals) %% max_unique == 0L)
    } else if (n_slots_new_vals == n_na_surplus) {
      return(TRUE)
    } else if ((length(unique_x) + n_unique_new_vals) %% frequency_max == 0L) {
      return(NA)
    } else {
      return(FALSE)
    }
  }

  # (These conditions aim to avoid a costly part, which is why the returns are
  # redundant:)
  # -- If their "count" is negative, it means the empty slots cannot be filled
  # with `NA`s, so `x` values differ in their true frequencies.
  # -- If their count (zero or positive) can be divided by the number of unique
  # values, these `NA`s might fill up the empty slots, so there is a possibility
  # that all values are equally frequent. By default (`exclusive = FALSE`), this
  # allows for separate sets of missing values and `NA` is returned; see the
  # previous if-else block.
  # -- Otherwise, there are some `NA`s that cannot be part of a group with the
  # modal frequency. They must be values with a lesser frequency, so there are
  # different frequencies among true `x` values.
  if (n_na_surplus < 0L) {
    FALSE
  } else if (
    n_na_surplus == 0L ||
    (is.null(max_unique) && n_na_surplus %% length(unique_x) == 0L)
  ) {
    NA
  } else {
    FALSE
  }
}

