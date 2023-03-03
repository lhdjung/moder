
# Actual modes ------------------------------------------------------------

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
#' # `2` is most frequent:
#' mode_first(c(1, 2, 2, 2, 3))
#'
#' # Can't determine the first mode --
#' # it might be `1` or `2` depending
#' # on the true value behind `NA:
#' mode_first(c(1, 1, 2, 2, NA))
#'
#' # Ignore `NA`s with `na.rm = TRUE`
#' # (there should be good reasons for this!):
#' mode_first(c(1, 1, 2, 2, NA), na.rm = TRUE)
#'
#' # `1` is the most frequent value,
#' # no matter what `NA` stands for:
#' mode_first(c(1, 1, 1, 2, NA))
#'
#' # Insist on finding the first mode,
#' # not just the first value known to
#' # be a mode, with `first_known = FALSE`:
#' mode_first(c(1, 2, 2, NA), first_known = FALSE)

mode_first <- function(x, na.rm = FALSE, first_known = TRUE) {
  # Iteration in the for loop will only
  # proceed on known `x` values:
  ix1 <- x[!is.na(x)]
  # Return `NA` early if required,
  # or remove `NA`s entirely if desired:
  if (!length(x) || all(is.na(x))) {
    return(NA)
  } else if (na.rm) {
    x <- ix1
  }
  frequency1 <- NULL
  for (i in seq_along(ix1)) {
    frequency1 <- c(frequency1, length(ix1[ix1 == ix1[[i]]]))
  }
  mode1 <- ix1[which.max(frequency1)]
  # The present implementation only differs
  # from the original function in terms
  # of `NA` handling. Therefore, it returns
  # `mode1` just like that function does
  # if there are no missing values:
  if (!any(is.na(x))) {
    return(mode1)
  }
  # What if some values really are missing?
  # The next few steps determine the number of
  # instances of `mode1` and the maximum number of
  # possible instances of the second-most frequent
  # value (i.e., with the count of all `NA`s added).
  # The goal is to test whether the latter might
  # contest `mode1`'s status as the first-appearing
  # mode:
  count_mode1 <- max(frequency1)
  count_mode2_na <- sort(unique(frequency1), decreasing = TRUE)
  if (length(count_mode2_na) > 1L) {
    count_mode2_na <- count_mode2_na[-1L]
  } else if (!length(count_mode2_na) || length(unique(ix1)) == 1L) {
    count_mode2_na <- 0L
  }
  count_mode2_na <- max(count_mode2_na) + length(x[is.na(x)])
  # Count unique modal values
  # (see explanation right below):
  count_modes_unique <- length(unique(ix1[frequency1 == max(frequency1)]))
  # The highest count -- that of `mode1` --
  # may decide the outcome right below.
  # If it's lower than the highest possible
  # count of any other value (`count_mode2_na`),
  # it's not known to be the mode. The same is
  # true if there is more than one unique mode
  # (because some values are unknown).
  # Otherwise, if the highest count is higher
  # than `count_mode2_na`, or if
  # `first_known = TRUE` (the default), `mode1`
  # is definitely the mode:
  if (count_mode1 < count_mode2_na || count_modes_unique > 1L) {
    return(methods::as(NA, typeof(x)))
  } else if (first_known || count_mode1 > count_mode2_na) {
    return(mode1)
  }
  # Check whether there is only a single unique known
  # value (i.e., `mode1`). If so, and if it's the
  # first value in `x`, `mode1` is the first mode
  # (because it's just as frequent as the next-most-
  # frequent value could possibly be). But if it only
  # appears after a missing value, it isn't:
  if (length(unique(ix1)) == 1L) {
    if (match(mode1, x) == 1L) {
      return(mode1)
    } else {
      return(methods::as(NA, typeof(x)))
    }
  }
  # Get the most frequent known value that is not `mode1`:
  frequency2 <- NULL
  x2 <- x[x != mode1]
  ix2 <- x2[!is.na(x2)]
  for (i in seq_along(ix2)) {
    frequency2 <- c(frequency2, length(ix2[ix2 == ix2[[i]]]))
  }
  # `NA` is returned if either there is no first value
  # or if `mode1` appears before `mode2` (i.e., if its
  # index of first occurrence is lower):
  mode2 <- x2[which.max(frequency2)]
  # Check whether `mode1` appears before `mode2` --
  # i.e., whether its index of first occurrence is lower:
  if (match(mode1, x) < match(mode2, x)) {
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
#'   the modes can't be determined because of missing values,
#'   returns `NA` instead.
#'
#' @export
#'
#' @seealso
#' - [mode_first()] for the first-appearing mode.
#' - [mode_single()] for the *only* mode, or `NA` if there are more.
#'
#' @examples
#' # Both `3` and `4` are the modes:
#' mode_all(c(1, 2, 3, 3, 4, 4))
#'
#' # Only `8` is:
#' mode_all(c(8, 8, 9))
#'
#' # Can't determine the modes here --
#' # `9` might be another mode:
#' mode_all(c(8, 8, 9, NA))
#'
#' # Either `1` or `2` could be a
#' # single mode, depending on `NA`:
#' mode_all(c(1, 1, 2, 2, NA))
#'
#' # `1` is the most frequent value,
#' # no matter what `NA` stands for:
#' mode_all(c(1, 1, 1, 2, NA))
#'
#' # Ignore `NA`s with `na.rm = TRUE`
#' # (there should be good reasons for this!):
#' mode_all(c(8, 8, 9, NA), na.rm = TRUE)
#' mode_all(c(1, 1, 2, 2, NA), na.rm = TRUE)

mode_all <- function(x, na.rm = FALSE) {
  # `NA`s are ignored at this point
  # because they will receive
  # special treatment later on:
  ix1 <- x[!is.na(x)]
  # Return `NA` early if required,
  # or remove `NA`s entirely if desired:
  if (!length(x) || all(is.na(x))) {
    return(NA)
  } else if (na.rm) {
    x <- ix1
  }
  # Determine the frequency of each
  # unique value in `x`:
  frequency1 <- NULL
  for (i in seq_along(ix1)) {
    frequency1 <- c(frequency1, length(ix1[ix1 == ix1[[i]]]))
  }
  # Subset the vector of unique known values
  # at the indices corresponding to the
  # most frequent known values:
  modes <- unique(ix1[frequency1 == max(frequency1)])
  # A seemingly unimodal distribution is
  # still subject to some `NA`-related
  # caveats. We call a helper function to
  # adjudicate whether the candidate mode
  # is certain to be the actual one or not:
  if (length(modes) == 1L) {
    decide_mode_na(x, unique(x[!is.na(x)]), modes)
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
#'   because of missing values, or if there is more than one mode,
#'   returns `NA` instead.
#'
#' @export
#'
#' @seealso
#' - [mode_first()] for the first-appearing mode.
#' - [mode_all()] for the full set of modes.
#'
#' @examples
#' # `8` is the only mode:
#' mode_single(c(8, 8, 9))
#'
#' # With more than one mode, the function
#' # returns `NA`:
#' mode_single(c(1, 2, 3, 3, 4, 4))
#'
#' # Can't determine the modes here --
#' # `9` might be another mode:
#' mode_single(c(8, 8, 9, NA))
#'
#' # `1` is the most frequent value,
#' # no matter what `NA` stands for:
#' mode_single(c(1, 1, 1, 2, NA))
#'
#' # Ignore `NA`s with `na.rm = TRUE`
#' # (there should be good reasons for this!):
#' mode_single(c(8, 8, 9, NA), na.rm = TRUE)

mode_single <- function(x, na.rm = FALSE) {
  # We need to check the number of
  # modes here, so we call `mode_all()`.
  # `na.rm` is `FALSE` here because, if
  # the user set it to `TRUE`, missing
  # values were removed already.
  mode1 <- mode_all(x, na.rm)
  # As the name says, if the distribution
  # has a single mode, return that value:
  if (length(mode1) == 1L) {
    mode1
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

mode_count <- function(x, na.rm = FALSE) {
  decide_count_na(mode_all(x, na.rm))
}



# Possible modes ----------------------------------------------------------

#' Possible modes
#'
#' @description `mode_possible_min()` and `mode_possible_max()` determine the
#'   minimal and maximal sets of modes from among known modes, given the number
#'   of missing values.
#'
#'   By default, the `_min` function assumes that at least one known value is a
#'   mode. Note that it never checks whether this is true.
#'
#' @param x A vector to search for its possible modes.
#' @param accept_known Boolean. Only in `mode_possible_min()`. If set to
#'   `FALSE`, the function returns `NA` if the set of all modes can't be
#'   determined due to missing values. Default is `TRUE`.
#'
#' @return A vector with the minimal or maximal possible modes (values tied for
#'   most frequent) in `x`. If the functions can't determine these possible
#'   modes because of missing values, they return `NA` instead.
#'
#' @export
#'
#' @seealso [mode_count_possible_min()] and [mode_count_possible_max()] for
#'   counting these possible modes.
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
#' # Only `7` can possibly be the mode
#' # because, even if `NA` is `8`, it's
#' # still less frequent than `7`:
#' mode_possible_min(c(7, 7, 7, 7, 8, 8, NA))
#' mode_possible_max(c(7, 7, 7, 7, 8, 8, NA))
#'
#' # No clear minimum of modes (because
#' # `NA` may tip the balance towards a
#' # single mode), but a clear maximum:
#' mode_possible_min(c(1, 1, 2, 2, NA))
#' mode_possible_max(c(1, 1, 2, 2, NA))

mode_possible_min <- function(x, accept_known = TRUE) {
  if (!accept_known) {
    return(mode_all(x, FALSE))
  }
  modes <- mode_all(x, TRUE)
  count_na <- length(x[is.na(x)])
  count_mode1 <- length(x[x == modes[[1L]] & !is.na(x)])
  if (count_na > count_mode1) {
    NA
  } else {
    modes
  }
}


#' @rdname mode-possible
#' @export

mode_possible_max <- function(x) {
  # The number of missings determines how far the
  # count of possible modes will go, and it will
  # be decremented as the process goes on:
  count_nas_left <- length(x[is.na(x)])
  x <- x[!is.na(x)]
  # No `NA`s mean no ambiguity about any
  # possible modes below the top level, so
  # the modes from this level are returned:
  modes <- mode_all(x, FALSE)
  if (count_nas_left == 0L) {
    return(modes)
  }
  # Initialize the vector of mode values.
  # These will be appended to the vector
  # from within the loop: one set of
  # mode values per level of modes.
  modes_out <- NULL
  # Run through the mode levels of `x`
  # for as long as there is a sufficient
  # amount of missing values left to fill
  # the "empty slots" of each lower level:
  while (count_nas_left > 0L) {
    # Determine the modes on the *current* level:
    modes <- mode_all(x, FALSE)
    # This vector will ultimately be returned,
    # but other values may be added to it:
    modes_out <- c(modes_out, unique(x[x %in% modes]))
    # Next *lower* level of modes:
    modes_next_level <- mode_all(x[!x %in% modes], FALSE)
    diff_length <- length(x[x %in% modes]) - length(x[x %in% modes_next_level])
    x <- x[!x %in% modes]
    count_empty_slots <- length(modes_next_level) * diff_length
    if (count_nas_left < count_empty_slots) {
      # Make sure to escape the loop because
      # there are not enough `NA`s left:
      count_nas_left <- 0L
    } else {
      # Append lower-level modes to the return vector:
      modes_out <- c(modes_out, unique(x[x == modes_next_level]))
      count_nas_left <- count_nas_left - max(count_empty_slots, 1L)
    }
  }
  # Finally, return the vector of unique possible modes --
  # or `NA` if there are none. This will likely only
  # occur if each input value is `NA`.
  if (length(modes_out)) {
    unique(modes_out)
  } else {
    NA
  }
}


#' Count possible modes
#'
#' @description `mode_count_possible_min()` and `mode_count_possible_max()`
#'   determine the minimal and maximal number of modes from among known modes,
#'   given the number of missing values.
#'
#'   They are wrappers around [mode_possible_min()] and [mode_possible_max()].
#'
#' @inheritParams mode-possible
#'
#' @return Integer. Minimal and maximal number of modes (values tied for most
#'   frequent) in `x`. If the functions can't determine these possible
#'   modes because of missing values, they return `NA` instead.
#'
#'
#' @export
#'
#' @seealso The examples for [mode_possible_min()] and [mode_possible_max()], to
#'   make it more clear how the present functions work.
#'
#' @name mode-count-possible

mode_count_possible_min <- function(x, accept_known = TRUE) {
  decide_count_na(mode_possible_min(x, accept_known))
}


#' @rdname mode-count-possible
#' @export

mode_count_possible_max <- function(x) {
  decide_count_na(mode_possible_max(x))
}



# Helper functions (not exported) -----------------------------------------

# Called within `mode_all()`:
decide_mode_na <- function(x, unique_x, mode1) {
  if (length(unique_x) == 1L) {
    if (length(x[is.na(x)]) < length(x) / 2) {
      return(mode1)
    } else {
      return(methods::as(NA, typeof(x)))
    }
  }
  ix2 <- x[x != mode1 & !is.na(x)]
  frequency2 <- NULL
  for (i in seq_along(ix2)) {
    frequency2 <- c(frequency2, length(ix2[ix2 == ix2[[i]]]))
  }
  mode2 <- ix2[which.max(frequency2)]
  count_na <- length(x[is.na(x)])
  x <- x[!is.na(x)]
  count_mode1 <- length(x[x == mode1])
  count_mode2_na <- length(x[x == mode2]) + count_na
  if (count_mode1 > count_mode2_na) {
    mode1
  } else {
    methods::as(NA, typeof(x))
  }
}


# Called within the counting functions:
# `mode_count()`, `mode_count_possible_min()`,
# and `mode_count_possible_max()`.
# If the set if modes can't be determined,
# the number of modes is an unknown integer.
# The length of `modes` is tested beforehand
# because this quickly rules out cases where
# `NA` testing is not necessary, and because
# multiple return values from `is.na()` would
# lead to an error in `if`:
decide_count_na <- function(modes) {
  if (length(modes) == 1L && is.na(modes)) {
    NA_integer_
  } else {
    length(modes)
  }
}

