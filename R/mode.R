#' The first-appearing mode
#'
#' `mode_first()` returns the mode that appears first in a vector, i.e., before
#' any other modes.
#'
#' @param x A vector to search for its first mode.
#' @param na.rm Boolean. Should missing values in `x` be removed before
#'   computation proceeds? Default is `FALSE`.
#' @param first_known Boolean. Should the first-appearing value known to be a
#'   mode be accepted? If `FALSE`, returns `NA` if a value that appears earlier
#'   might be another mode due to missing values. Default is `TRUE`.
#'
#' @return The first mode (most frequent value) in `x`. If it can't be
#'   determined because of missing values, returns `NA` instead.
#'
#' @export
#'
#' @seealso
#' - [mode_all()] for the full set of modes.
#' - [mode_single()] for the *only* mode, or `NA` if there are more.
#'
#' @examples
mode_first <- function(x, na.rm = FALSE, first_known = TRUE) {
  # Remove `NA`s if desired:
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # We determine the unique known values
  # of `x`. `NA`s are ignored at this point
  # because they will receive special treatment
  # later on.
  ux <- unique(x[!is.na(x)])
  # Count the instances of each known value:
  tab <- tabulate(match(x, ux))
  # Subset the most frequent known value --
  # the putative mode; either this or `NA`
  # will ultimately be returned:
  mode1 <- ux[which.max(tab)]
  # The present implementation only differs
  # from the original function in terms
  # of `NA` handling. Therefore, it returns
  # `mode1` just like that function does
  # if there are no missing values:
  if (!any(is.na(x))) {
    return(mode1)
  }
  # What if some values really are missing?
  # Unlike the other mode functions, `mode_first()`
  # needs to check whether the first-appearing value
  # (or, by default of `first_known = TRUE`, the
  # first-appearing value that is known to be a mode)
  # is as frequent or more as all other values taken
  # together.
  # The next few steps determine the maximum number
  # of possible instances of the second-most
  # frequent value. The goal is to test whether
  # this value might contest `mode1`' s status
  # as the first-appearing mode:
  count_mode2_na <- sort(tab, decreasing = TRUE)[-1L]
  if (!length(count_mode2_na)) {
    count_mode2_na <- 0L
  }
  count_mode2_na <- max(count_mode2_na) + length(x[is.na(x)])
  # By default, `first_known = TRUE` lowers
  # the threshold that `mode1` needs to pass
  # by 1. That is because it accepts the first
  # known mode even if that value appears only
  # after the possible other mode, `mode2`:
  if (first_known) {
    count_mode2_na <- count_mode2_na - 1
  }
  if (max(tab) > count_mode2_na) {
    return(mode1)
  }
  # `mode_first()` is agnostic about other modes.
  # By default (`first_known = TRUE`), it returns
  # either the first-appearing value that is known
  # to be a mode or `NA`. That's a pragmatic default.
  # If a value might be a mode depending on true values
  # behind `NA`s, and it appears before the first `mode1`
  # value, the function still returns `mode1`.
  # Set `first_known` to `FALSE` to make the function
  # return `NA` in such cases. The idea is that, strictly
  # speaking, the true first mode is unknown here.
  # For example, `mode_first(c(2, 1, 1, NA))` returns `1`
  # although `2` appears first -- and `2` might be another
  # mode, but the first value that is known to be a mode
  # is `1`. `mode_first(c(2, 1, 1, NA), first_known = FALSE)`
  # returns `NA`.
  # Get the most frequent known value that is not `mode1`:
  mode2 <- ux[which.max(tabulate(match(x[x != mode1], ux)))]
  mode1_appears_first <- match(mode1, x) < match(mode2, x)
  mode1_is_half_or_more <- max(tab) >= length(x) / 2
  if (mode1_is_half_or_more && (mode1_appears_first || first_known)) {
    mode1
    # Call a helper function that adjudicates
    # whether or not it's still possible to
    # determine the mode:
  } else if (is.na(decide_mode_na(x, ux, mode1))) {
    methods::as(NA, typeof(x))
  } else if (length(ux) == 1L && mode1_is_half_or_more) {
    mode1
  } else {
    methods::as(NA, typeof(x))
  }
}


#' All modes
#'
#' `mode_all()` returns the set of all modes in a vector.
#'
#' @param x A vector to search for its modes.
#' @inheritParams mode_first
#'
#' @return A vector with all modes (values tied for most frequent) in `x`. If
#'   the modes can't be determined because of missing values, returns `NA`
#'   instead.
#'
#' @export
#'
#' @seealso
#' - [mode_first()] for the first-appearing mode.
#' - [mode_single()] for the *only* mode, or `NA` if there are more.
#'
#' @examples
mode_all <- function(x, na.rm = FALSE) {
  # Remove `NA`s if desired:
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # As above, we determine the unique
  # known values of `x`. `NA`s are ignored
  # at this point because they will receive
  # special treatment later on.
  ux <- unique(x[!is.na(x)])
  # Count the instances of each known value:
  tab <- tabulate(match(x, ux))
  # Subset the vector of unique known values
  # at the indices corresponding to the
  # most frequent known values:
  modes <- ux[tab == max(tab)]
  # A seemingly unimodal distribution is
  # subject to the `NA`-related caveats
  # described in `mode_first()`, so we call
  # the same `NA` helper as that function.
  # However, we don't allow for ties between
  # the `modes` count and the sum of the
  # second-most-frequent value and `NA` counts
  # (`FALSE` at the end) because such a tie
  # means that the true set of modes is
  # unknown -- all `NA`s might stand for
  # the second-most frequent value, after all!
  if (length(modes) == 1L) {
    decide_mode_na(x, ux, modes)
    # Any missing value could mask any of the
    # known values tied for most frequent --
    # and break the tie. This makes it
    # impossible to determine the true set
    # of modes, so the function returns `NA`:
  } else if (any(is.na(x))) {
    methods::as(NA, typeof(x))
    # Multimodal distributions without `NA`s
    # have a clearly determined set of modes:
  } else {
    modes
  }
}


#' The single mode
#'
#' `mode_single()` returns the only mode in a vector, or `NA` if there are
#' multiple modes.
#'
#' @param x A vector to search for its mode.
#' @inheritParams mode_first
#'
#' @return The only mode (most frequent value) in `x`. If it can't be determined
#'   because of missing values, or if there is more than one mode, returns `NA`
#'   instead.
#'
#' @export
#'
#' @seealso
#' - [mode_first()] for the first-appearing mode.
#' - [mode_all()] for the full set of modes.
#'
#' @examples
mode_single <- function(x, na.rm = FALSE) {
  # Remove `NA`s if desired:
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # We need to check the number of
  # modes here, so we call `mode_all()`.
  # `na.rm` is `FALSE` here because, if
  # the user set it to `TRUE`, missing
  # values were removed already.
  mode1 <- mode_all(x, FALSE)
  # As the name says, if the distribution
  # has a single mode (that passes the
  # `NA` test), that value is returned.
  # `NA` testing without allowing for ties
  # between the `mode1` count and the sum
  # of the `mode2` and `NA` counts
  # (`FALSE` at the end) is necessary here
  # because we need to make sure that
  # `mode1` is really the only mode, even
  # if all `NA`s stand in for `mode2`
  # (the latter is computed within
  # `decide_mode_na()`):
  if (length(mode1) == 1L) {
    decide_mode_na(x, unique(x[!is.na(x)]), mode1)
    # Multimodal distributions are always `NA`.
    # Some users prefer this stricter way of
    # estimating the mode, or they require it
    # for their specific use cases.
  } else {
    NA
  }
}


#' Number of modes
#'
#' `mode_count()` counts the modes in a vector. Thin wrapper around
#' [`mode_all()`].
#'
#' @inheritParams mode_all
#'
#' @return Integer. Number of modes in `x`. If the modes can't be determined
#'   because of missing values, returns `NA` instead.
#'
#' @export
#'
#' @examples
mode_count <- function(x, na.rm = FALSE) {
  modes <- mode_all(x, na.rm)
  # This condition needs to be wrapped in `all()` because multiple modes would
  # otherwise lead to an error:
  if (all(is.na(modes))) {
    NA_integer_
  } else {
    length(modes)
  }
}


# Helper function; not exported but called within all exported functions:
decide_mode_na <- function(x, ux, mode1) {
  if (length(ux) == 1L) {
    if (length(x[is.na(x)]) < length(x) / 2) {
      return(mode1)
    } else {
      return(methods::as(NA, typeof(x)))
    }
  }
  mode2 <- ux[which.max(tabulate(match(x[x != mode1], ux)))]
  count_mode1 <- length(x[x == mode1])
  count_mode2_na <- length(x[x == mode2]) + length(x[is.na(x)])
  if (count_mode1 > count_mode2_na) {
    mode1
  } else {
    methods::as(NA, typeof(x))
  }
}


# decide_mode_na <- function(x, ux, mode1, allow_tie) {
#   # Some values might be missing. We need to check
#   # whether there are so many missings that the most
#   # frequent known value, `mode1`, might be less
#   # frequent than the second-most frequent one (or
#   # the first value tied with `mode1` but appearing
#   # after it) if all the `NA`s mask the latter.
#   # To do so, we need to find this second value.
#   # We look for a possible mode much like above,
#   # but this time, we exclude the `mode1` values:
#   mode2 <- ux[which.max(tabulate(match(x[x != mode1], ux)))]
#   # Count instances of the three relevant available
#   # values -- most and second-most frequent known
#   # values plus missing values:
#   count_mode1 <- length(x[x == mode1])
#   count_mode2_na <- length(x[x == mode2]) + length(x[is.na(x)])
#   # `mode_first()` only require `mode1` to be
#   # at least as frequent as the sum of the
#   # other two counts. That is because it is
#   # only interested in getting the first mode,
#   # whether or not there are other modes
#   # beyond that. However, `mode_all()` and
#   # `mode_single()` are more strict because
#   # they are meant to rule out that the true
#   # count of `mode2` is just as high as that
#   # of `mode1`. If this is even possible,
#   # these functions need to return `NA` --
#   # `mode_all()` because it needs to be sure
#   # about the set of modes, and `mode_single()`
#   # because it only allows for one mode.
#   if (allow_tie) {
#     mode1_frequent_enough <- count_mode1 >= count_mode2_na
#   } else {
#     mode1_frequent_enough <- count_mode1 > count_mode2_na
#   }
#   # (Assuming `allow_tie = TRUE`:)
#   # `mode1` is the true mode only if it's
#   # at least as frequent as the sum of the
#   # other two counts. Otherwise, if all the
#   # `NA`s mask `mode2` values, the true count
#   # of `mode2` would be higher than that of
#   # `mode1`. We don't know which values hide
#   # behind `NA`, so we can't rule out this
#   # second scenario if the known count of
#   # `mode1` is lower than a possibly true
#   # count of `mode2`. Therefore, if
#   # `count_mode1` is not large enough, the
#   # function returns `NA` (coerced to the
#   # same type as the input, `x`):
#   if (mode1_frequent_enough) {
#     mode1
#   } else {
#     methods::as(NA, typeof(x))
#   }
# }
