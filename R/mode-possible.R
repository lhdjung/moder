#' Possible sets of modes
#'
#' @description `mode_possible_min()` and `mode_possible_max()` determine the
#'   minimal and maximal sets of modes, given the number of missing values.
#'
#' @param x A vector to search for its possible modes.
#' @param accept Logical. If `accept` is set to `TRUE`, the functions don't
#'   necessarily return the minimal or maximal sets of modes but all values that
#'   *might* be part of those sets. Default is `FALSE`. See details.
#' @param multiple Deprecated; will be removed in the future. Old name of
#'   `accept`.
#'
#' @details If `accept = TRUE`, the functions return multiple values that may or
#'   may not be modes depending on the true values behind `NA`s. Why is this
#'   disabled by default? In cases where multiple unique values would be modes
#'   if and only if one or more missing values represented them but there are
#'   not enough missing values to represent all of them, any values that are not
#'   represented by enough `NA`s would not be modes. This makes it unclear which
#'   unique values are part of the minimal and maximal sets of modes, so the
#'   default of `accept` is to return `NA` in these cases.
#'
#' @return Vector of the same type as `x`. By default, it contains the minimal
#'   or maximal possible sets of modes (values tied for most frequent) in `x`.
#'   If the functions can't determine these possible modes because of missing
#'   values, they return `NA`.
#'
#' @export
#'
#' @seealso [mode_count_range()] for the minimal and maximal *numbers* of
#'   possible modes. They can always be determined, even if the present
#'   functions return `NA`.
#'
#' @name mode-possible
#'
#' @examples
#' # "a" is guaranteed to be a mode,
#' # "b" might also be one, but
#' # "c" is impossible:
#' mode_possible_min(c("a", "a", "a", "b", "b", "c", NA))
#' mode_possible_max(c("a", "a", "a", "b", "b", "c", NA))
#'
#' # Only `8` can possibly be the mode
#' # because, even if `NA` is `7`, it's
#' # still less frequent than `8`:
#' mode_possible_min(c(7, 7, 8, 8, 8, 8, NA))
#' mode_possible_max(c(7, 7, 8, 8, 8, 8, NA))
#'
#' # No clear minimal or maximal set
#' # of modes because `NA` may tip
#' # the balance between `1` and `2`
#' # towards a single mode:
#' mode_possible_min(c(1, 1, 2, 2, 3, 4, 5, NA))
#' mode_possible_max(c(1, 1, 2, 2, 3, 4, 5, NA))
#'
#' # With `accept = TRUE`, the functions
#' # return all values that might be part of
#' # the min / max sets of modes; not these
#' # sets themselves:
#' mode_possible_min(c(1, 1, 2, 2, 3, 4, 5, NA), accept = TRUE)
#' mode_possible_max(c(1, 1, 2, 2, 3, 4, 5, NA), accept = TRUE)

mode_possible_min <- function(x, accept = FALSE, multiple = NULL) {
  # `multiple` is deprecated:
  if (!missing(multiple)) {
    mode_possible_warn_multiple()
  }
  # Without missing values, the minimal set of modes is simply the actual one:
  n_x <- length(x)
  x <- x[!is.na(x)]
  n_na <- n_x - length(x)
  if (n_na == 0L) {
    return(mode_all_if_no_na(x))
  }
  # Otherwise, the minimum might be the set of modes among known values:
  mode1 <- mode_all_if_no_na(x)
  x_without_mode1 <- x[!x %in% mode1]
  # This is a corner case with just one known value:
  if (length(x_without_mode1) == 0L && length(mode1) == 1L) {
    if (length(x[x == mode1[[1L]]]) < n_na) {
      return(x[NA_integer_])
    } else {
      return(mode1)
    }
  }
  # (See below.)
  mode2 <- mode_all(x_without_mode1)
  n_mode1 <- length(x[x == mode1[[1L]]])
  n_mode2_na <- length(x[x %in% mode2[[1L]]]) + n_na
  # The next-most-frequent known values plus `NA`s must not be more frequent
  # than `mode1`, or the latter isn't guaranteed to be a minimum of modes. The
  # same is true if there are two or more `mode1` values, because `NA`s can make
  # any of these more frequent than the others:
  if (n_mode2_na > n_mode1 || (length(mode1) > 1L && !accept)) {
    x[NA_integer_]
  } else {
    mode1
  }
}


#' @rdname mode-possible
#' @export

mode_possible_max <- function(x, accept = FALSE, multiple = NULL) {
  # `multiple` is deprecated:
  if (!missing(multiple)) {
    mode_possible_warn_multiple()
  }
  # The number of missings determines how far the count of possible modes will
  # go, and it will be decremented as the process goes on:
  n_x <- length(x)
  x <- x[!is.na(x)]
  n_nas_left <- n_x - length(x)
  # No `NA`s mean no ambiguity about any possible modes below the top level, so
  # the modes from this level are returned:
  modes <- mode_all_if_no_na(x)
  if (n_nas_left == 0L) {
    return(modes)
  }
  # Initialize the vector of mode values. These will be appended to the vector
  # from within the loop: one set of mode values per level of modes.
  modes_out <- NULL
  # Also initialize a vector that will keep track of the maximum frequency, and
  # that may resolve a corner case:
  n_max <- NULL
  # Run through the mode levels of `x` for as long as there is a sufficient
  # amount of missing values left to fill the "empty slots" of each lower level:
  while (n_nas_left > 0L) {
    # Determine the modes on the *current* level. This requires `mode_all()`
    # because the faster `mode_all_if_no_na()` can't take `numeric(0)`
    # arguments, which might occur here.
    modes <- mode_all(x, FALSE)
    # This vector will ultimately be returned, but other values may be added to
    # it:
    modes_out <- c(modes_out, unique(x[x %in% modes]))
    # Next *lower* level of modes:
    modes_next_level <- mode_all(x[!x %in% modes], FALSE)
    n_modes <- length(x[x %in% modes[[1L]]])
    n_modes_next_level <- length(x[x %in% modes_next_level[[1L]]])
    n_max <- max(n_max, n_modes)
    x <- x[!x %in% modes]
    n_empty_slots <- length(modes_next_level) * (n_modes - n_modes_next_level)
    # In case the remaining `NA`s can't fill up the empty slots, there won't be
    # another loop cycle:
    if (n_nas_left < n_empty_slots) {
      # With multiple next-most-frequent values (which is not accepted by
      # default of `accept`) and some `NA`s remaining (but not enough; see right
      # above) and the possibility that some of the multiples might be actual
      # modes if combined with all remaining `NA`s, there is no clear maximum
      # set of modes because the `NA`s make it unclear which of the
      # next-most-frequent values might be as frequent as the top-level ones.
      # This returns `NA` for the same reason as `mode_all(c(1, 1, 2, 2, NA))`.
      if (accept && n_modes_next_level + n_nas_left >= n_max) {
        next
      } else if (
        length(modes_next_level) > 1L &&
        n_nas_left > 0L &&
        n_modes_next_level + n_nas_left >= n_max
      ) {
        return(x[NA_integer_])
      } else {
        # Escape from the loop because there are not enough `NA`s left:
        break
      }
    } else {
      # In this case, the empty slots can be filled. Append lower-level modes to
      # the return vector:
      modes_out <- c(modes_out, unique(x[x == modes_next_level]))
      n_nas_left <- n_nas_left - max(n_empty_slots, 1L)
      # If there is more than one mode per level and there are not enough `NA`s
      # to fill all of them up, there is a pseudo-tie that can be broken by
      # `NA`s, so there is no clear maximum in this case:
      if (length(modes) > 1L && n_nas_left < n_empty_slots && !accept) {
        return(x[NA_integer_])
      }
    }
  }
  # Finally, return the vector of unique possible modes -- or `NA` if there are
  # none. This will likely only occur if each input value is `NA`.
  if (length(modes_out)) {
    unique(modes_out)
  } else {
    x[NA_integer_]
  }
}

