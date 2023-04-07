
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
#'   mode be accepted? If `FALSE` (the default), returns `NA` if a value that
#'   appears earlier might be another mode due to missing values.
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
#' # By default, the function insists on
#' # the first mode, so it won't accept the
#' # first value *known* to be a mode if an
#' # earlier value might be a mode, too:
#' mode_first(c(1, 2, 2, NA))
#'
#' # Accept the first-known mode with
#' # `first_known = TRUE`:
#' mode_first(c(1, 2, 2, NA), first_known = TRUE)

mode_first <- function(x, na.rm = FALSE, first_known = FALSE) {
  # Iteration in the for loop will only
  # proceed on known `x` values:
  ix1 <- x[!is.na(x)]
  # Return `NA` early if required,
  # or remove `NA`s entirely if desired:
  if (length(x) == 0L || all(is.na(x))) {
    return(x[NA_integer_])
  } else if (na.rm) {
    x <- ix1
  }
  frequency1 <- vapply(ix1, function(x) length(ix1[ix1 == x]), 1L)
  mode1 <- ix1[which.max(frequency1)]
  # The present implementation only differs
  # from the original function in terms
  # of `NA` handling. Therefore, it returns
  # `mode1` just like that function does
  # if there are no missing values:
  if (!anyNA(x)) {
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
  } else if (length(count_mode2_na) == 0L || length(unique(ix1)) == 1L) {
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
  # than `count_mode2_na`, `mode1` is definitely
  # the mode. It's also accepted as such if
  # `first_known = TRUE` because it's known to be
  # a mode, even if an earlier value is also one:
  if (count_mode1 < count_mode2_na || count_modes_unique > 1L) {
    return(x[NA_integer_])
  } else if (count_mode1 > count_mode2_na || first_known) {
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
      return(x[NA_integer_])
    }
  }
  # Get the most frequent known value that is not `mode1`:
  # frequency2 <- NULL
  x2 <- x[x != mode1]
  ix2 <- x2[!is.na(x2)]
  frequency2 <- vapply(ix2, function(x) length(ix2[ix2 == x]), 1L)
  # `NA` is returned if either there is no first value
  # or if `mode1` appears before `mode2` (i.e., if its
  # index of first occurrence is lower):
  mode2 <- x2[which.max(frequency2)]
  # Check whether `mode1` appears before `mode2` --
  # i.e., whether its index of first occurrence is lower:
  if (match(mode1, x) < match(mode2, x)) {
    mode1
  } else {
    x[NA_integer_]
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
  if (length(x) == 0L || all(is.na(x))) {
    return(x[NA_integer_])
  } else if (na.rm) {
    x <- ix1
  }
  # Determine the frequency of each
  # unique value in `x`:
  frequency1 <- vapply(ix1, function(x) length(ix1[ix1 == x]), 1L)
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
    decide_mode_na(x, unique(ix1), modes)
    # Any missing value could mask any of the
    # known values tied for most frequent --
    # and break the tie. This makes it
    # impossible to determine the true set
    # of modes, so the function returns `NA`:
  } else if (anyNA(x)) {
    x[NA_integer_]
    # Multimodal distributions without `NA`s
    # have a clearly determined set of modes:
  } else {
    modes
  }
}


#' The single mode
#'
#' `mode_single()` returns the only mode in a vector. If there are multiple
#' modes, it returns `NA` by default.
#'
#' @param x A vector to search for its mode.
#' @param multiple String or integer (length 1), or a function. What to do if
#'   `x` has multiple modes. The default returns `NA`. All other options rely on
#'   the modal values: "`min"`, `"max"`, `"mean"`, `"median"`, `"first"`,
#'   `"last"`, and `"random"`. Alternatively, `multiple` can be an index number,
#'   or a function that summarizes the modes. See details.
#' @inheritParams mode_first
#'
#' @return The only mode (most frequent value) in `x`. If it can't be determined
#'   because of missing values, `NA` is returned instead. By default, `NA` is
#'   also returned if there are multiple modes (`multiple = "NA"`).
#'
#' @details If `x` is a string vector and `multiple` is `"min"` or `"max"`, the
#'   mode is selected lexically, just like `min(letters)` returns `"a"`. The
#'   `"mean"` and `"median"` options return `NA` with a warning. For factors,
#'   `"min"`, `"max"`, and `"median"` are errors, but `"mean"` returns `NA` with
#'   a warning. These are inconsistencies in base R.
#'
#'   The `multiple` options `"first"` and `"last"` always select the mode that
#'   appears first or last in `x`. Index numbers, like `multiple = 2`, allow you
#'   to select more flexibly. If `multiple` is a function, its output must be
#'   length 1.
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

mode_single <- function(x, na.rm = FALSE, multiple = "NA") {
  # The `multiple` argument will usually be a string.
  # It should then be checked against the vector of
  # possible string shorthands for dealing with
  # multiple modes, using an intermediate variable
  # to avoid redundant printing of each string in
  # case of an error:
  if (is.character(multiple)) {
    possible_strings <-
      c("NA", "min", "max", "mean", "median", "first", "last", "random")
    match.arg(multiple, possible_strings)
    rm(possible_strings)
  }
  # We need to check the number of
  # modes here, so we call `mode_all()`:
  modes <- mode_all(x, na.rm)
  # As the name says, if the distribution
  # has a single mode, return that value:
  if (length(modes) == 1L) {
    modes
    # Multimodal distributions are `NA` by default.
    # Some users prefer this stricter way of
    # estimating the mode, or they require it for
    # their specific use cases.
  } else if (is.character(multiple)) {
    # Execute the user's chosen strategy
    # for dealing with multiple modes,
    # or the "NA" default:
    switch(
      multiple,
      "NA"      = x[NA_integer_],
      "min"     = min(modes),
      "max"     = max(modes),
      "mean"    = mean(modes),
      "median"  = stats::median(modes),
      "first"   = modes[1L],
      "last"    = modes[length(modes)],
      "random"  = sample(modes, size = 1L)
    )
  } else if (is.numeric(multiple)) {
    # Index numbers must not be greater than
    # the number of modes, which is an error:
    if (multiple > length(modes)) {
      msg_error <- paste("`multiple` is", multiple, "but there are only")
      msg_error <- paste(msg_error, length(modes), "modes")
      stop(msg_error)
    }
    modes[multiple]
  } else {
    # The user may also specify `multiple`
    # as an object that can be interpreted
    # as a function to manually summarize
    # `modes`:
    modes <- as.function(multiple)(modes)
    # The output of the user-supplied
    # function has to be of length 1
    # to keep `mode_single()` true to
    # itself, or else there will be
    # an error:
    if (length(modes) == 1L) {
      modes
    } else {
      msg_error <- "Function supplied to `multiple` returns object of length"
      msg_error <- paste(msg_error, length(modes), "instead of 1")
      stop(msg_error)
    }
  }
}


#' Modal frequency
#'
#' @description Call `mode_frequency()` to get the number of times that a
#'   vector's mode appears in the vector.
#'
#'   See [mode_frequency_range()] for bounds on an unknown frequency.
#'
#' @param x A vector to check for its modal frequency.
#' @inheritParams mode_first
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

mode_frequency <- function(x, na.rm = FALSE) {
  # The modal frequency can't be determined if
  # any values are missing because each of them
  # might increase the frequency:
  if (na.rm || !anyNA(x)) {
    # If the mode can be determined, count
    # its occurrences among non-`NA` values.
    # This requires one mode of `x`, so we call
    # `mode_first()` with `first_known = TRUE`
    # (because position is irrelevant here):
    mode <- mode_first(x, na.rm, TRUE)
    length(x[x == mode & !is.na(x)])
  } else {
    NA_integer_
  }
}


#' Modal count
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

#' Possible sets of modes
#'
#' @description `mode_possible_min()` and `mode_possible_max()` determine the
#'   minimal and maximal sets of modes from among known modes, given the number
#'   of missing values.
#'
#' @param x A vector to search for its possible modes.
#'
#' @return A vector with the minimal or maximal possible modes (values tied for
#'   most frequent) in `x`. If the functions can't determine these possible
#'   modes because of missing values, they return `NA` instead.
#'
#' @export
#'
#' @seealso [mode_count_range()] for counting these possible modes.
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
#' # of modes (because `NA` may tip
#' # the balance towards a single mode):
#' mode_possible_min(c(1, 1, 2, 2, 3, 4, 5, NA))
#' mode_possible_max(c(1, 1, 2, 2, 3, 4, 5, NA))

mode_possible_min <- function(x) {
  # Without missing values, the minimal
  # set of modes is simply the actual one:
  count_na <- length(x[is.na(x)])
  if (count_na == 0L) {
    return(mode_all(x, FALSE))
  }
  # Otherwise, the minimum might be
  # the set of modes among known values:
  x <- x[!is.na(x)]
  mode1 <- mode_all(x, FALSE)
  x_without_mode1 <- x[!x %in% mode1]
  # This is a corner case with just one
  # known value:
  if (length(x_without_mode1) == 0L && length(mode1) == 1L) {
    if (length(x[x == mode1[[1L]]]) < count_na) {
      return(x[NA_integer_])
    } else {
      return(mode1)
    }
  }
  # (See below.)
  mode2 <- mode_all(x_without_mode1)
  count_mode1 <- length(x[x == mode1[[1L]]])
  count_mode2_na <- length(x[x %in% mode2[[1L]]]) + count_na
  # The next-most-frequent known values
  # plus `NA`s must not be more frequent
  # than `mode1`, or the latter isn't
  # guaranteed to be a minimum of modes.
  # The same is true if there are two or
  # more `mode1` values, because `NA`s
  # can make any of these more frequent
  # than the others:
  if (count_mode2_na > count_mode1 || length(mode1) > 1L) {
    x[NA_integer_]
  } else {
    mode1
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
  # Also initialize a vector that will
  # keep track of the maximum frequency,
  # and that may resolve a corner case:
  count_max <- NULL
  # Run through the mode levels of `x`
  # for as long as there is a sufficient
  # amount of missing values left to fill
  # the "empty slots" of each lower level:
  while (count_nas_left > 0L) {
    # Determine the modes on the *current* level:
    modes <- mode_all(x, FALSE)
    # More than one mode per level means
    # there is a pseudo-tie that can be
    # broken by `NA`s, so there is no
    # clear maximum in this case:
    if (length(modes) > 1L) {
      return(x[NA_integer_])
    }
    # This vector will ultimately be returned,
    # but other values may be added to it:
    modes_out <- c(modes_out, unique(x[x %in% modes]))
    # Next *lower* level of modes:
    modes_next_level <- mode_all(x[!x %in% modes], FALSE)
    count_modes <- length(x[x %in% modes[[1L]]])
    count_modes_next_level <- length(x[x %in% modes_next_level[[1L]]])
    count_diff <- count_modes - count_modes_next_level
    count_max <- max(count_max, count_modes)
    x <- x[!x %in% modes]
    count_empty_slots <- length(modes_next_level) * count_diff
    # In case the remaining `NA`s can't
    # fill up the empty slots, there
    # won't be another loop cycle:
    if (count_nas_left < count_empty_slots) {
      # With multiple next-most-frequent values
      # and some `NA`s remaining (but not enough;
      # see right above) as well as the possibility
      # that some of the multiples might be actual
      # modes if combined with all remaining `NA`s,
      # there is no clear maximum set of modes
      # because the `NA`s make it unclear which
      # of the next-most-frequent values might
      # be as frequent as the top-level ones.
      # This returns `NA` for the same reason
      # that `mode_all(c(1, 1, 2, 2, NA))` does.
      if (
        length(modes_next_level) > 1L &&
        count_nas_left > 0L &&
        count_modes_next_level + count_nas_left >= count_max
      ) {
        return(x[NA_integer_])
      } else {
        # Escape from the loop because
        # there are not enough `NA`s left:
        break
      }
    } else {
      # In this case, the empty slots can be filled.
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
    x[NA_integer_]
  }
}


#' Modal count range
#'
#' @description `mode_count_range()` determines the minimal and maximal number
#'   of modes from among known modes, given the number of missing values.
#'
#' @inheritParams mode-possible
#'
#' @return Integer (length 2). Minimal and maximal number of modes (values tied
#'   for most frequent) in `x`. If these possible modescan't be determined
#'   because of missing values, the function returns `NA` instead.
#'
#' @export
#'
#' @seealso The examples for [mode_possible_min()] and [mode_possible_max()], to
#'   make it more clear how the present function works. (It wraps these two.)

mode_count_range <- function(x) {
  count_min <- decide_count_na(mode_possible_min(x))
  count_max <- decide_count_na(mode_possible_max(x))
  c(count_min, count_max)
}


#' Modal frequency range
#'
#' @description Call `mode_frequency_range()` to get lower and upper bounds for
#'   the number of times that a vector's mode appears in the vector. The lower
#'   bound assumes that no `NA`s are the mode; the upper bound assumes that all
#'   `NA`s are.
#'
#' @inheritParams mode_frequency
#'
#' @return Integer (length 2). If there are no `NA`s in `x`, the two integer
#'   values are identical. If all `x` values are `NA`, the values are `1` (no
#'   two values are the same) and the total number of values (all values are the
#'   same).
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

mode_frequency_range <- function(x) {
  # This will be decreased by the
  # number of known values:
  count_na <- length(x)
  x <- x[!is.na(x)]
  # If all values are missing,
  # the range is very uncertain:
  if (length(x) == 0L) {
    return(c(1L, count_na))
  }
  # This is now the true `NA` count:
  count_na <- count_na - length(x)
  # Minimum modal frequency: exclude all `NA`s
  # (they were removed above)
  # Maximum modal frequency: include all `NA`s
  # (add their count to the minimum)
  frequency1 <- mode_frequency(x, FALSE)
  c(frequency1, frequency1 + count_na)
}



# Helper functions (not exported) -----------------------------------------

# Called within `mode_all()`:
decide_mode_na <- function(x, unique_x, mode1) {
  if (length(unique_x) == 1L) {
    if (length(x[is.na(x)]) < length(x) / 2) {
      return(mode1)
    } else {
      return(x[NA_integer_])
    }
  }
  ix2 <- x[x != mode1 & !is.na(x)]
  frequency2 <- vapply(ix2, function(x) length(ix2[ix2 == x]), 1L)
  mode2 <- ix2[which.max(frequency2)]
  count_na <- length(x[is.na(x)])
  x <- x[!is.na(x)]
  count_mode1 <- length(x[x == mode1])
  count_mode2_na <- length(x[x == mode2]) + count_na
  if (count_mode1 > count_mode2_na) {
    mode1
  } else {
    x[NA_integer_]
  }
}


# Called within the counting functions:
# `mode_count()` and `mode_count_range()`.
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

