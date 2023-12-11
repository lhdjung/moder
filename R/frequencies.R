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
#' @details By default (`na.rm = FALSE`), the function returns `NA` if any
#'   values are missing. That is because missings make the frequency uncertain
#'   even if the mode is known: any missing value may or may not be the mode,
#'   and hence count towards the modal frequency.
#'
#' @return Integer (length 1) or `NA`.
#'
#' @seealso [mode_first()], which the function wraps.
#'
#' @export
#'
#' @examples
#' # The mode, `9`, appears three times:
#' mode_frequency(c(7, 8, 8, 9, 9, 9))
#'
#' # With missing values, the frequency
#' # is unknown, even if the mode isn't:
#' mode_frequency(c(1, 1, NA))
#'
#' # You can ignore this problem and
#' # determine the frequency among known values
#' # (there should be good reasons for this!):
#' mode_frequency(c(1, 1, NA), na.rm = TRUE)

mode_frequency <- function(x, na.rm = FALSE, max_unique = NULL) {
  n_x <- length(x)
  x <- x[!is.na(x)]
  n_na <- n_x - length(x)
  if (is.null(max_unique)) {
    check_factor_max_unique(x, n_na, "mode_frequency")
  }
  # The modal frequency can't be determined if any values are missing because
  # each of them might increase the frequency:
  if (na.rm || n_na == 0L) {
    # If the mode can be determined, count its occurrences among non-`NA`
    # values. This requires one mode of `x`, so we call `mode_first()` with
    # `accept = TRUE` (because position is irrelevant here):
    mode <- mode_first(x, na.rm, accept = TRUE)
    return(length(x[x == mode]))
  } else if (is.null(max_unique)) {
    return(NA_integer_)
  }
  unique_x <- unique(x)
  max_unique <- handle_max_unique_input(
    x, max_unique, length(unique_x), n_na, "mode_frequency"
  )
  if (
    n_na == 1L &&
    max_unique == length(unique_x) &&
    all(unique_x %in% mode_all_if_no_na(x))
  ) {
    as.integer(length(x) / length(unique_x) + 1)
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
#' @details If there are no `NA`s in `x`, the two return values are identical.
#'   If all `x` values are `NA`, the return values are `1` (no two `x` values
#'   are the same) and the total number of values (all `x` values are the same).
#'
#' @return Integer (length 2).
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
  # If all values are missing, the range is highly uncertain (see docs):
  if (length(x) == 0L) {
    return(c(1L, n_x))
  }
  if (!is.null(max_unique)) {
    unique_x <- unique(x)
    max_unique <- handle_max_unique_input(
      x, max_unique, length(unique_x), n_na, "mode_frequency_range"
    )
    if (
      any(c(n_na, length(unique_x)) == 1L) &&
      max_unique == length(unique_x) &&
      all(unique_x %in% mode_all_if_no_na(x))
    ) {
      return(c(1L, 1L))
    }
  }
  # -- Minimum frequency: exclude all `NA`s (they were removed above)
  # -- Maximum frequency: include all `NA`s (add their count to the minimum)
  frequency_min <- mode_frequency(x)
  frequency_max <- frequency_min + n_na
  c(frequency_min, frequency_max)
}

