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
  # The next few steps determine the number of
  # instances of `mode1` and the maximum number of
  # possible instances of the second-most frequent
  # value (i.e., with the count of all `NA`s added).
  # The goal is to test whether the latter might
  # contest `mode1`'s status as the first-appearing
  # mode:
  count_mode1 <- max(tab)
  count_mode2_na <- sort(tab, decreasing = TRUE)[-1L]
  if (!length(count_mode2_na)) {
    count_mode2_na <- 0L
  }
  count_mode2_na <- max(count_mode2_na) + length(x[is.na(x)])
  # The highest count -- that of `mode1` --
  # may decide the outcome right below.
  # If it's lower than the highest possible
  # count of any other value (`count_mode2_na`),
  # it's not known to be the mode.
  # But if it's higher than that value, or if
  # `first_known = TRUE` (the default), it is:
  if (count_mode1 < count_mode2_na) {
    return(methods::as(NA, typeof(x)))
  } else if (first_known || count_mode1 > count_mode2_na) {
    return(mode1)
  }
  # Check whether there is only a single unique known
  # value (i.e., `mode1` ). If so, and if it's the
  # first value in `x`, `mode1` is the first mode
  # (because it's just as frequent as the next-most-
  # frequent value could possibly be). But if it only
  # appears after a missing value, it isn't:
  if (length(ux) == 1L) {
    if (match(mode1, x) == 1L) {
      return(mode1)
    } else {
      return(methods::as(NA, typeof(x)))
    }
  }
  # Get the most frequent known value that is not `mode1`:
  mode2 <- ux[which.max(tabulate(match(x[x != mode1], ux)))]
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
  # still subject to some `NA`-related
  # caveats. We call a helper function to
  # adjudicate whether the candidate mode
  # is certain to be the actual one or not:
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
#'   the modes can't be determined because of missing values, returns `NA`
#'   instead.
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
  modes <- mode_all(x, na.rm)
  # If the set if modes can't be determined,
  # the number of modes is an unknown integer.
  # The length of `modes` is tested beforehand
  # because it quickly rules out cases where
  # `NA` testing is not necessary, and because
  # multiple return values from `is.na()` would
  # lead to an error in `if`:
  if (length(modes) == 1L && is.na(modes)) {
    NA_integer_
  } else {
    length(modes)
  }
}


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
    message("NO MISSINGS; EARLY RETURN")
    return(modes)
  }
  # Initialize the vector of mode values.
  # These will be appended to the vector
  # from within the loop: one set of
  # mode values per level of modes.
  modes_out <- NULL
  while (count_nas_left > 0L) {
    # Determine the modes on the *current* level:
    modes <- mode_all(x, TRUE)
    # This vector will ultimately be returned,
    # but other values may be added to it:
    modes_out <- c(modes_out, unique(x[x %in% modes]))
    # Next *lower* level of modes:
    modes_next_level <- mode_all(x[!x %in% modes], TRUE)
    diff_length <- length(x[x %in% modes]) - length(x[x %in% modes_next_level])
    x <- x[!x %in% modes]
    # modes_next_level <- mode_all(x[!x %in% modes], TRUE)
    count_empty_slots <- length(modes_next_level) * diff_length
    if (count_nas_left < count_empty_slots) {
      message("MISSINGS COUNT SET TO ZERO")
      count_nas_left <- 0L
    } else {
      message("SUBTRACTING EMPTY SLOTS COUNT FROM MISSINGS COUNT")
      modes_out <- c(modes_out, unique(x[x == modes_next_level]))
      count_nas_left <- count_nas_left - max(count_empty_slots, 1L)
    }
    # # At the end of the loop, check if there are
    # # any other values if `x`. If there aren't any,
    # # there is no point in another run of the loop:
    # if (length(x[!x %in% modes & !x %in% modes_next_level]) == 0L) {
    #   message("NO OTHER VALUES FOUND")
    #   # modes_out <- c(modes_out, x[x == modes_next_level[1L]][1L])
    #   return(modes_out)
    # }
  }
  unique(modes_out)
}



# Helper function; not exported but called within `mode_all()`:
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

