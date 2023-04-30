#' The first-appearing mode
#'
#' `mode_first()` returns the mode that appears first in a vector, i.e., before
#' any other modes.
#'
#' @param x A vector to search for its first mode.
#' @param na.rm Boolean. Should missing values in `x` be removed before
#'   computation proceeds? Default is `FALSE`.
#' @param accept Boolean. Should the first-appearing value known to be a mode be
#'   accepted? If `FALSE` (the default), returns `NA` if a value that appears
#'   earlier might be another mode due to missing values.
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
#' # You may accept the first-known mode:
#' mode_first(c(1, 2, 2, NA), accept = TRUE)

mode_first <- function(x, na.rm = FALSE, accept = FALSE) {
  # Iteration in the for loop will only proceed on known `x` values:
  ix1 <- x[!is.na(x)]
  # Return `NA` early if required, or remove `NA`s entirely if desired:
  if (length(x) == 0L || all(is.na(x))) {
    return(x[NA_integer_])
  } else if (na.rm) {
    x <- ix1
  }
  frequency1 <- vapply(ix1, function(x) length(ix1[ix1 == x]), 1L)
  mode1 <- ix1[which.max(frequency1)]
  # The present implementation only differs from the original function in terms
  # of `NA` handling. Therefore, it returns `mode1` just like that function does
  # if there are no missing values:
  if (!anyNA(x)) {
    return(mode1)
  }
  # What if some values really are missing? The next few steps determine the
  # number of instances of `mode1` and the maximum number of possible instances
  # of the second-most frequent value (i.e., with the count of all `NA`s added).
  # The goal is to test whether the latter might contest `mode1`'s status as the
  # first-appearing mode:
  n_mode1 <- max(frequency1)
  n_mode2_na <- sort(unique(frequency1), decreasing = TRUE)
  if (length(n_mode2_na) > 1L) {
    n_mode2_na <- n_mode2_na[-1L]
  } else if (length(n_mode2_na) == 0L || length(unique(ix1)) == 1L) {
    n_mode2_na <- 0L
  }
  n_mode2_na <- max(n_mode2_na) + length(x[is.na(x)])
  # Count unique modal values (see explanation right below):
  n_modes_unique <- length(unique(ix1[frequency1 == max(frequency1)]))
  # The highest count -- that of `mode1` -- may decide the outcome right below.
  # If it's lower than the highest possible count of any other value
  # (`n_mode2_na`), it's not known to be the mode. The same is true if there is
  # more than one unique mode (because some values are unknown). Otherwise, if
  # the highest count is higher than `n_mode2_na`, `mode1` is definitely the
  # mode. It's also accepted as such if `accept = TRUE` because it's known
  # to be a mode, even if an earlier value is also one:
  if (n_mode1 < n_mode2_na || n_modes_unique > 1L) {
    return(x[NA_integer_])
  } else if (n_mode1 > n_mode2_na || accept) {
    return(mode1)
  }
  # Check whether there is only a single unique known value (i.e., `mode1`). If
  # so, and if it's the first value in `x`, `mode1` is the first mode (because
  # it's just as frequent as the next-most- frequent value could possibly be).
  # But if it only appears after a missing value, it isn't:
  if (length(unique(ix1)) == 1L) {
    if (match(mode1, x) == 1L) {
      return(mode1)
    } else {
      return(x[NA_integer_])
    }
  }
  # Get the most frequent known value that is not `mode1`:
  x2 <- x[x != mode1]
  ix2 <- x2[!is.na(x2)]
  frequency2 <- vapply(ix2, function(x) length(ix2[ix2 == x]), 1L)
  # `NA` is returned if either there is no first value or if `mode1` appears
  # before `mode2` (i.e., if its index of first occurrence is lower):
  mode2 <- x2[which.max(frequency2)]
  # Check whether `mode1` appears before `mode2` -- i.e., whether its index of
  # first occurrence is lower:
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
  # `NA`s are ignored at this point because they will receive special treatment
  # later on:
  ix1 <- x[!is.na(x)]
  # Return `NA` early if required, or remove `NA`s entirely if desired:
  if (length(x) == 0L || all(is.na(x))) {
    return(x[NA_integer_])
  } else if (na.rm) {
    x <- ix1
  }
  # Determine the frequency of each unique value in `x`:
  frequency1 <- vapply(ix1, function(x) length(ix1[ix1 == x]), 1L)
  # Subset the vector of unique known values at the indices corresponding to the
  # most frequent known values:
  modes <- unique(ix1[frequency1 == max(frequency1)])
  # A seemingly unimodal distribution is still subject to some `NA`-related
  # caveats. We call a helper function to adjudicate whether the candidate mode
  # is certain to be the actual one or not:
  if (length(modes) == 1L) {
    decide_mode_na(x, unique(ix1), modes)
    # Any missing value could mask any of the known values tied for most
    # frequent -- and break the tie. This makes it impossible to determine the
    # true set of modes, so the function returns `NA`:
  } else if (anyNA(x)) {
    x[NA_integer_]
    # Multimodal distributions without `NA`s have a clearly determined set of
    # modes:
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
#' @param accept Boolean. Should the minimum set of modes be accepted (to select
#'   the single mode from)? If set to `FALSE`, insists on the complete set and
#'   returns `NA` if it can't be determined. Default is `TRUE`.
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
#' @details If `accept` is set to `FALSE`, the set of modes is obtained via
#'   `mode_all()` instead of the default `mode_possible_min()`. The purpose of
#'   the default is to avoid returning `NA` when some, though not all modes are
#'   known. Set it to `FALSE` if it's important to select the single mode from
#'   the complete set of modes.
#'
#'   If `x` is a string vector and `multiple` is `"min"` or `"max"`, the mode is
#'   selected lexically, just like `min(letters)` returns `"a"`. The `"mean"`
#'   and `"median"` options return `NA` with a warning. For factors, `"min"`,
#'   `"max"`, and `"median"` are errors, but `"mean"` returns `NA` with a
#'   warning. These are inconsistencies in base R.
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
#' - [mode_all()] for the complete set of modes.
#' - [mode_possible_min()] for the minimal set of modes.
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
#' # Accept `8` anyways if it's
#' # sufficient to just have any mode:
#' mode_single(c(8, 8, 9, NA), accept = TRUE)
#'
#' # `1` is the most frequent value,
#' # no matter what `NA` stands for:
#' mode_single(c(1, 1, 1, 2, NA))
#'
#' # Ignore `NA`s with `na.rm = TRUE`
#' # (there should be good reasons for this!):
#' mode_single(c(8, 8, 9, NA), na.rm = TRUE)

mode_single <- function(x, na.rm = FALSE, accept = TRUE, multiple = "NA") {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (is.character(multiple)) {
    match.arg(
      multiple,
      c("NA", "min", "max", "mean", "median", "first", "last", "random")
    )
  }
  # By default (`accept = TRUE`), it is sufficient that at least one mode is
  # known, as opposed to all modes:
  modes <- if (accept) {
    mode_possible_min(x)
  } else {
    mode_all(x)
  }
  # As the name says, if the distribution has a single mode, return that value:
  if (length(modes) == 1L) {
    modes
    # Multimodal distributions are `NA` by default. Some users prefer this
    # stricter way of estimating the mode, or they require it for their specific
    # use cases.
  } else if (is.character(multiple)) {
    # Execute the user's chosen strategy for dealing with multiple modes, or the
    # "NA" default:
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
    # Index numbers must not be greater than the number of modes, which is an
    # error:
    if (multiple > length(modes)) {
      msg_error <- paste("`multiple` is", multiple, "but there are only")
      msg_error <- paste(msg_error, length(modes), "modes")
      stop(msg_error)
    }
    modes[multiple]
  } else {
    # The user may also specify `multiple` as an object that can be interpreted
    # as a function to manually summarize `modes`:
    modes <- as.function(multiple)(modes)
    # The output of the user-supplied function has to be of length 1 to keep
    # `mode_single()` true to itself, or else there will be an error:
    if (length(modes) == 1L) {
      modes
    } else {
      msg_error <- "Function supplied to `multiple` returns object of length"
      msg_error <- paste(msg_error, length(modes), "instead of 1")
      stop(msg_error)
    }
  }
}
