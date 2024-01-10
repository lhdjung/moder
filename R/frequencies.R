#' Modal frequency
#'
#' @description Call `mode_frequency()` to get the number of times that a
#'   vector's mode appears in the vector.
#'
#'   See [mode_frequency_range()] for bounds on an unknown frequency.
#'
#' @param x A vector to check for its modal frequency.
#' @inheritParams mode_first
#' @inheritParams mode_is_trivial
#'
#' @details By default, the function returns `NA` if any values are missing.
#'   That is because missings make the frequency uncertain even if the mode is
#'   known: any missing value may or may not be an instance of the mode, and
#'   hence count towards the modal frequency.
#'
#' @return Integer (length 1).
#'
#' @seealso [mode_first()], which the function wraps.
#'
#' @export
#'
#' @examples
#' # The mode, `9`, appears three times:
#' mode_frequency(c(7, 8, 8, 9, 9, 9))
#'
#' # Missing values make the frequency
#' # unknown, even if the mode is obvious:
#' mode_frequency(c(1, 1, NA))
#'
#' # You can ignore this problem and
#' # determine the frequency among known values
#' # (there should be good reasons for this!):
#' mode_frequency(c(1, 1, NA), na.rm = TRUE)
#'
#' # In a multimodal distribution, the modal
#' # frequency is that of a single mode:
#' mode_frequency(c(3, 3, 4, 4, 5, 5, 6))

mode_frequency <- function(x, na.rm = FALSE, max_unique = NULL) {
  n_x <- length(x)
  x <- x[!is.na(x)]
  if (na.rm) {
    n_x <- length(x)
    n_na <- 0L
  } else {
    n_na <- n_x - length(x)
  }
  if (is.null(max_unique)) {
    check_factor_max_unique(x, n_na, "mode_frequency")
  }
  # Two important cases to check for:
  # -- No values are missing, so the frequency can be determined by finding one
  # mode (using a specialized internal helper) and counting its instances.
  # -- By default, the modal frequency can't be determined if any values are
  # missing because each of them may or may not increase the frequency.
  # -- Some values are missing and the number of unique values is not limited by
  # `max_unique`. This means the modal frequency is unknown.
  if (n_na == 0L) {
    mode1 <- mode_first_if_no_na(x)
    return(length(x[x == mode1]))
  } else if (is.null(max_unique)) {
    return(NA_integer_)
  }
  unique_x <- unique(x)
  max_unique <- handle_max_unique_input(
    x, max_unique, length(unique_x), n_na, "mode_frequency"
  )
  # An edge case occurs if there is only one `NA`, it must represent a known
  # value (as per `max_unique`), and all known values are equally frequent.
  # Since the `NA` must count towards one of the known values, the modal
  # frequency is known to be the modal frequency among known values plus one
  # `NA`. Otherwise, `NA_integer_` is returned because the frequency is unknown:
  # either multiple `NA`s are distributed across the known values in unknown
  # ways, or it is unclear whether a single `NA` represents a known or an
  # unknown value, or even the known values are non-uniformly distributed.
  frequency_is_known <- n_na == 1L &&
    max_unique == length(unique_x) &&
    all(unique_x %in% mode_all_if_no_na(x))
  # Arithmetically, integer division (`%/%`) works like regular division here
  # because `length(x)` will always be divisible by `length(unique_x)`. The only
  # difference is that the output is integer, as it should be.
  if (frequency_is_known) {
    length(x) %/% length(unique_x) + 1L
  } else {
    NA_integer_
  }
}


#' Modal frequency range
#'
#' @description `mode_frequency_range()` determines the minimum and maximum
#'   number of times that a vector's mode appears in the vector. The minimum
#'   assumes that no `NA`s are the mode; the maximum assumes that all `NA`s are.
#'
#' @inheritParams mode_frequency
#'
#' @return Integer (length 2). If there are no `NA`s in `x`, the two elements
#'   are identical because the modal frequency is precisely known.
#'
#' @export
#'
#' @seealso [mode_frequency()], for the precise frequency (or `NA` if it can't
#'   be determined).
#'
#' @examples
#' # The mode is `7`. It appears four or
#' # five times because the `NA` might
#' # also be a `7`:
#' mode_frequency_range(c(7, 7, 7, 7, 8, 8, NA))
#'
#' # All of `"c"`, `"d"`, and `"e"` are the modes,
#' # and each of them appears twice:
#' mode_frequency_range(c("a", "b", "c", "c", "d", "d", "e", "e"))

mode_frequency_range <- function(x, max_unique = NULL) {
  if (is.null(max_unique)) {
    check_factor_max_unique(x, n_na, "mode_frequency_range")
  }
  n_x <- length(x)
  x <- x[!is.na(x)]
  n_na <- n_x - length(x)
  if (!is.null(max_unique)) {
    unique_x <- unique(x)
    max_unique <- handle_max_unique_input(
      x, max_unique, length(unique_x), n_na, "mode_frequency_range"
    )
    frequency_is_1 <- max_unique == length(unique_x) &&
      (n_na == 1L || length(unique_x) == 1L) &&
      all(unique_x %in% mode_all_if_no_na(x))
    if (frequency_is_1) {
      return(c(1L, 1L))
    }
  }
  # Special rules apply if there are no known values:
  # -- If there are no `NA`s either, there are no values at all, so the modal
  # frequency is known to be zero.
  # -- Otherwise, all values are `NA`, so the minimal frequency is 1 (no two `x`
  # values are the same) and the maximal frequency is the total number of values
  # (all `x` values are the same).
  if (length(x) == 0L) {
    if (n_na == 0L) {
      return(c(0L, 0L))
    } else {
      return(c(1L, n_x))
    }
  }
  # -- Minimum frequency: exclude all `NA`s (they were removed above)
  # -- Maximum frequency: include all `NA`s (add their count to the minimum)
  mode1 <- mode_first_if_no_na(x)
  frequency_min <- length(x[x == mode1])
  c(frequency_min, frequency_min + n_na)
}
