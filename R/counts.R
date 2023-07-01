#' Modal count
#'
#' `mode_count()` counts the modes in a vector. Thin wrapper around
#' [`mode_all()`].
#'
#' @inheritParams mode_all
#' @inheritParams mode_is_trivial
#'
#' @return Integer. Number of modes (values tied for most frequent) in `x`. If
#'   the modes can't be determined because of missing values,
#'   returns `NA` instead.
#'
#' @export
#'
#' @examples
#' # There are two modes, `3` and `4`:
#' mode_count(c(1, 2, 3, 3, 4, 4))
#'
#' # Only one mode, `8`:
#' mode_count(c(8, 8, 9))
#'
#' # Can't determine the number of modes
#' # here -- `9` might be another mode:
#' mode_count(c(8, 8, 9, NA))
#'
#' # Either `1` or `2` could be a
#' # single mode, depending on `NA`:
#' mode_count(c(1, 1, 2, 2, NA))
#'
#' # `1` is the most frequent value,
#' # no matter what `NA` stands for:
#' mode_count(c(1, 1, 1, 2, NA))
#'
#' # Ignore `NA`s with `na.rm = TRUE`
#' # (there should be good reasons for this!):
#' mode_count(c(8, 8, 9, NA), na.rm = TRUE)
#' mode_count(c(1, 1, 2, 2, NA), na.rm = TRUE)

mode_count <- function(x, na.rm = FALSE, max_unique = NULL) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n_x <- length(x)
  x <- x[!is.na(x)]
  n_na <- n_x - length(x)
  rm(n_x)
  if (is.null(max_unique)) {
    check_factor_max_unique(x, n_na, "mode_count")
  } else {
    unique_x <- unique(x)
    max_unique <- handle_max_unique_input(
      x, max_unique, length(unique_x), n_na, "mode_count"
    )
    if (n_na == 1L && all(unique_x %in% mode_all_if_no_na(x))) {
      if (max_unique == length(unique_x)) {
        return(1L)
      } else {
        return(NA_integer_)
      }
    } else {
      rm(unique_x, max_unique, na.rm)
    }
  }
  modes <- mode_all(c(x, rep(x[NA_integer_], times = n_na)))
  if (length(modes) == 1L && is.na(modes)) {
    NA_integer_
  } else {
    length(modes)
  }
}


#' Modal count range
#'
#' @description `mode_count_range()` determines the minimal and maximal number
#'   of modes given the number of missing values.
#'
#' @inheritParams mode-possible
#' @inheritParams mode_is_trivial
#'
#' @details If `x` is a factor, `max_unique` should be `"known"` or there is a
#'   warning. This is because a factor's levels are supposed to include all of
#'   its possible values.
#'
#' @return Integer (length 2). Minimal and maximal number of modes (values tied
#'   for most frequent) in `x`.
#'
#' @export
#'
#' @examples
#' # If `NA` is `7` or `8`, that number is
#' # the only mode; otherwise, both numbers
#' # are modes:
#' mode_count_range(c(7, 7, 8, 8, NA))
#'
#' # Same result here -- `7` is the only mode
#' # unless `NA` is secretly `8`, in which case
#' # there are two modes:
#' mode_count_range(c(7, 7, 7, 8, 8, NA))
#'
#' # But now, there is no way for `8` to be
#' # as frequent as `7`:
#' mode_count_range(c(7, 7, 7, 7, 8, 8, NA))
#'
#' # The `NA`s might form a new mode here
#' # if they are both, e.g., `9`:
#' mode_count_range(c(7, 7, 8, 8, NA, NA))
#'
#' # However, if there can be no values beyond
#' # those already known -- `7` and `8` --
#' # the `NA`s can't form a new mode.
#' # Specify this with `max_unique = "known"`:
#' mode_count_range(c(7, 7, 8, 8, NA, NA), max_unique = "known")

mode_count_range <- function(x, max_unique = NULL) {
  n_x <- length(x)
  x <- x[!is.na(x)]
  n_na <- n_x - length(x)
  rm(n_x)
  n_unique_x <- length(unique(x))
  if (is.null(max_unique)) {
    check_factor_max_unique(x, n_na, "mode_count_range")
  } else {
    max_unique <- handle_max_unique_input(
      x, max_unique, n_unique_x, n_na, "mode_count_range"
    )
  }
  # Throw an error if `max_unique` was specified as too low a number:
  if (!is.null(max_unique) && max_unique < n_unique_x) {
    msg_error <- paste("`max_unique` is", max_unique, "but there are")
    msg_error <- paste(msg_error, n_unique_x, "values in `x`")
    stop(msg_error)
  }
  # The minimal and maximal mode counts are identical if there are no `NA`s:
  if (n_na == 0L) {
    n_exact <- suppressWarnings(mode_count(x, FALSE))
    return(rep(n_exact, times = 2L))
  }
  # This part works like the helper `count_slots_empty()`, but it involves
  # variables that are used elsewhere in the present function:
  frequency_max <- length(x[x %in% mode_first_if_no_na(x)])
  n_slots_all <- n_unique_x * frequency_max
  n_slots_empty <- n_slots_all - length(x)
  # Prepare an early return if there is no way for all known values to be modes:
  if (n_na < n_slots_empty) {
    frequency_all <- table(x)
    for (i in seq_along(frequency_all)) {
      if (frequency_all[i] < frequency_max) {
        diff_to_max <- frequency_max - frequency_all[i]
        if (n_na < diff_to_max) {
          return(c(1L, length(frequency_all[frequency_all == frequency_max])))
        } else {
          frequency_all[i] <- frequency_max
          n_na <- n_na - diff_to_max
        }
      }
    }
  }
  # What if the `NA`s cannot represent any values other than those already
  # known, or if there is a known limit to the number of values they can
  # represent?
  if (!is.null(max_unique)) {
    # How many `NA`s remain after filling up all the empty slots with other
    # `NA`s?
    n_na_surplus <- n_na - n_slots_empty
    if (n_na_surplus < 0L) {
      return(c(1L, n_unique_x + n_na_surplus))
    }
    n_na_new_vals <- frequency_max * (max_unique - n_unique_x)
    if (n_na_new_vals < frequency_max && n_na_surplus %% n_unique_x == 0L) {
      return(c(1L, n_unique_x))
    } else if (n_na_new_vals < n_na_surplus) {
      n_na_beyond_new_vals <- (n_na_surplus - n_na_new_vals) %% max_unique
      if (n_na_beyond_new_vals == 0L) {
        return(c(1L, n_unique_x + (n_na_new_vals / frequency_max)))
      } else {
        return(c(1L, n_na_beyond_new_vals))
      }
    } else if (n_na_new_vals == n_na_surplus) {
      return(c(1L, max_unique))
    } else {
      return(c(1L, n_unique_x + n_na_new_vals %/% frequency_max))
    }
  }
  # How many slots can be filled at the maximum?
  n_slots_max <- length(x) + n_na
  # Implicit condition: Some values are missing, and their number is greater
  # than or equal to the number of empty slots.
  c(1L, n_slots_max %/% frequency_max)
}

