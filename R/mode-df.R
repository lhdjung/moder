#' Tabulate mode estimates with the certainty about them
#'
#' @description `mode_df()` takes a data frame (or another list) of numeric
#'   vectors and computes the mode or modes of each element. Where the true mode
#'   is unknown due to missing values, more and more `NA`s are ignored until an
#'   estimate for the mode is found.
#'
#'   Estimates are presented along with information about whether they are known
#'   to be the true mode, how many `NA`s had to be ignored during estimation,
#'   the rate of ignored `NA`s, etc.
#'
#' @param x List of vectors. Each vector needs to be numeric or similar. Note
#'   that data frames are lists, so `x` can be a data frame.
#' @param method String. How to determine the mode(s)? Options are:
#'   - `"first"` for [`mode_first()`], the default.
#'   - `"all"` for [`mode_all()`]. This may return multiple values per estimate.
#'   - `"single"` for [`mode_single()`]. The only option that can return `NA`
#'   estimates.
#' @param na.rm.from String. Only relevant to the default `method = "first"`.
#'   Where to start when removing `NA`s from `x`? Options are `"start"`,
#'   `"end"`, and `"random"`. Default is `"start"`.
#' @param accept Passed on to [`mode_first()`] and [`mode_single()`]. Default is
#'   `FALSE`.
#' @param multiple Passed on to [`mode_single()`]. Default is `"NA"`.
#'
#' @details The function deals with missing values (`NA`s) by first checking
#'   whether they make the true mode unknown. If they do, it removes one `NA`,
#'   then checks again; and so on until an estimate is found.
#'
#'   This strategy is based on the `na.rm.amount` argument of [`mode_first()`],
#'   [`mode_all()`], and [`mode_single()`]. It represents a middle way between
#'   simply ignoring all `NA`s and not even trying to compute an estimate.
#'   Instead, it only removes the minimum number of `NA`s necessary, because
#'   some distributions have a known mode (or set of modes) even if some of
#'   their values are missing. By keeping track of the removed `NA`s,
#'   `mode_df()` quantifies the uncertainty about its estimates.
#'
#' @return Tibble (data frame) with these columns:
#'   - `term`: the names of `x` elements. Only present if any are named.
#'   - `estimate`: the modes of `x` elements, ignoring as many `NA`s as
#'   necessary. List-column if `method = "all"`.
#'   - `certainty`: `TRUE` if the corresponding estimate is certain to be the
#'   true mode, and `FALSE` if this is unclear due to missing values.
#'   - `na_ignored`: the number of missing values that had to be ignored to
#'   arrive at the estimate.
#'   - `na_total`: the total number of missing values.
#'   - `rate_ignored_na`: the proportion of missing values that had to be
#'   ignored from among all missing values.
#'   - `sum_total`: the total number of values, missing or not.
#'   - `rate_ignored_sum`: the proportion of missing values that had to be
#'   ignored from among all values, missing or not.
#'
#' @export
#'
#' @include mode-proper.R
#'
#' @examples
#' # Use a list of numeric vectors:
#' my_list <- list(
#'   a = 1:15,
#'   b = c(1, 1, NA),
#'   c = c(4, 4, NA, NA, NA, NA),
#'   d = c(96, 24, 3, NA)
#' )
#'
#' mode_df(my_list)
#'
#' # Data frames are allowed:
#' mode_df(iris[1:4])


# # Test with:
# x <- list(
#   a = 1:15,
#   b = c(1, 1, NA),
#   c = c(4, 4, NA, NA, NA, NA),
#   d = c(96, 24, 3, NA)
# )
# method <- "first"
# na.rm.amount <- 0
# na.rm.from <- c("first", "last", "random")
# accept <- FALSE
# multiple <- "NA"


mode_df <- function(x, method = c("first", "all", "single"),
                    na.rm.from = c("first", "last", "random"), accept = FALSE,
                    multiple = c("NA", "min", "max", "mean", "median",
                                 "first", "last", "random")) {
  # Check that `x` is a list because the point of this function is to find
  # estimates for the mode of each element of `x`:
  if (!is.list(x)) {
    stop("`x` must be a list.")
  }
  # Check specifications of the strategy arguments:
  method <- match.arg(method)
  na.rm.from <- match.arg(na.rm.from)
  multiple <- match.arg(multiple)
  # Initialize the two most important output vectors. They will be columns of
  # the output data frame. The `estimate` vector is a list if the method is
  # `"all"` because `mode_all()` can return length > 1 vectors. Otherwise,
  # `estimate` is integer for the corner case that all values that will be
  # assigned to its elements are integers (i.e., none is a double; this would
  # coerce any integer elements).
  length_x <- length(x)
  na_ignored <- integer(length_x)
  estimate <- if (method == "all") {
    vector("list", length_x)
  } else {
    integer(length_x)
  }
  # The `mode_*()` function called below is determined by the `method` argument:
  name_fn <- paste0("mode_", method)
  # Loop through the `x` elements, attempting to find a mode estimate for each
  # by removing one `NA` at a time:
  for (i in seq_len(length_x)) {
    # Vectors where all elements are missing have an unknown mode, so the
    # estimate should be `NA`, as well (of the same type as `x[[i]]`). This is
    # implemented via a shortcut:
    if (all(is.na(x[[i]]))) {
      estimate[[i]] <- x[[i]][NA_integer_]
      next
    }
    # Initialize the number of `NA`s that need to be ignored at zero. This value
    # will be incremented once for every failed attempt to get a non-`NA`
    # estimate. At the start, however, the true mode of a given element of `x`
    # may yet be known.
    na_ignored_current <- 0L
    # Keep looking for a modal estimate for `x[[i]]`. This is implemented via an
    # infinite loop. In other words, `repeat` is equivalent to `while (TRUE)`:
    repeat {
      # Call the mode function chosen via the `method` argument, then call it
      # with its specific list of arguments:
      estimate_current <- do.call(
        what = name_fn,
        args = switch(
          method,
          "first" = list(
            x = x[[i]], na.rm = FALSE, na.rm.amount = na_ignored_current,
            na.rm.from = na.rm.from, accept = accept
          ),
          "all" = list(
            x = x[[i]], na.rm = FALSE, na.rm.amount = na_ignored_current
          ),
          "single" = list(
            x = x[[i]], na.rm = FALSE, na.rm.amount = na_ignored_current,
            accept = accept, multiple = multiple
          )
        )
      )
      # Check whether `estimate_current` is `NA`, excluding a dangerous edge
      # case: `mode_single()` was called with its `multiple == "NA"` default,
      # but all `NA`s in `x[[i]]` are already ignored. This additional check can
      # be necessary to prevent an infinite loop because `mode_single()` returns
      # `NA` by default if there are multiple modes.
      estimate_is_na <- length(estimate_current) == 1L &&
        is.na(estimate_current) &&
        !(
          method == "single" &&
            multiple == "NA" &&
            na_ignored_current >= length(x[[i]][is.na(x[[i]])])
        )
      # -- If the estimate is `NA`, there will be (at least) one more iteration
      # of the repeat loop, and it will ignore one more `NA`.
      # -- If a non-`NA` estimate has been found, enter it into the `estimate`
      # vector, record the number of `NA`s that needed to be ignored for this
      # result, and break out of the repeat loop.
      if (estimate_is_na) {
        na_ignored_current <- na_ignored_current + 1L
      } else {
        na_ignored[i] <- na_ignored_current
        estimate[[i]] <- estimate_current
        break
      }
    } # End of repeat loop
  }   # End of for loop
  # As a purely mechanical consequence of the `na_ignored` integer vector's
  # values being zero or greater, `certainty` marks those cases where no `NA`s
  # need to be ignored to compute an estimate; and thus, where there is a known
  # and determinate mode:
  certainty <- na_ignored == 0L
  # Count the missing values in each element of `x`, then compute the rate of
  # missing values that had to be ignored (to find a mode estimate) as a share
  # of the total number of missing values:
  na_total <- vapply(
    x, function(y) length(y[[1L]][is.na(y)]), integer(1L), USE.NAMES = FALSE
  )
  # Compute the rates of ignored `NA`s from all `NA`s and from all values:
  sum_total <- vapply(x, length, integer(1L), USE.NAMES = FALSE)
  rate_ignored_na <- na_ignored / na_total
  rate_ignored_sum <- na_ignored / sum_total
  # Collect the length-`x` vectors in a data frame, adding a `term` column in
  # case any elements of `x` are named:
  tibble::tibble(
    term = names(x), estimate, certainty, na_ignored, na_total, rate_ignored_na,
    sum_total, rate_ignored_sum
  )
}
