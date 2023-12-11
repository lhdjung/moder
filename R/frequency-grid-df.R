#' Frequency grid data frame
#'
#' @description NOTE: This function is currently experimental and shouldn't be
#'   relied upon.
#'
#'   `frequency_grid_df()` takes a vector and creates an extended frequency
#'   table about it. Internally, this is used as a basis for
#'   `frequency_grid_plot()`.
#'
#' @param x A vector.
#' @inheritParams mode_is_trivial
#'
#' @return A data frame with these columns:
#' - `x`: The input vector, with each unique known value repeated to be as
#'   frequent as the most frequent one.
#' - `freq` (integer): Hypothetical frequency of each `x` value.
#' - `is_missing` (logical): Is the observation absent from the input vector?
#' - `can_be_filled` (logical): Are there enough `NA`s so that one of them might
#'   hypothetically represent the `x` value in question, implying that there
#'   would be at least as many observations of that value as the respective
#'   frequency (`freq`) indicates?
#' - `is_supermodal` (logical): Is the frequency of this value greater than the
#'   maximum frequency among known values?
#'
#' @section Limitations: See the limitations section of `frequency_grid_plot()`.
#'
#' @export
#'
#' @examples
#' x <- c("a", "a", "a", "b", "b", "c", NA, NA, NA, NA, NA)
#' frequency_grid_df(x)

frequency_grid_df <- function(x, max_unique = NULL) {
  n_x <- length(x)
  x <- sort(x[!is.na(x)])
  n_na <- n_x - length(x)
  freq_table <- as.data.frame(
    table(x), responseName = "freq", stringsAsFactors = FALSE
  )
  freq <- integer(length = sum(freq_table$freq))
  index_out <- 1L
  for (i in seq_len(nrow(freq_table))) {
    i_freq  <- freq_table[i, ][[2L]]
    i_range <- index_out:(index_out + i_freq - 1L)
    freq[i_range] <- seq_along(i_range)
    index_out <- index_out + i_freq
  }

  rm(freq_table, i_freq, i_range, index_out)

  if (is.factor(x)) {
    x <- as.character(x)
  }
  unique_x <- unique(x)
  freq_max_known <- max(freq)

  # For the `max_unique` argument:
  max_unique <- handle_max_unique_input(
    x, max_unique, length(unique_x), n_na, "frequency_grid_df"
  )

  n_slots_empty <- freq_max_known * length(unique_x) - length(x)
  n_na_surplus <- n_na - n_slots_empty

  # TODO: Fix this whole if-else block! Maybe put `freq_diff` to the end; it's
  # the difference between `freq_max_known` and the "supermode".
  freq_diff <- 0L
  if (is.null(max_unique)) {
    # max_unique <- max_unique %/% freq_max_known
  } else if (max_unique == length(unique_x)) {
    # START of the `max_unique = "known"`-assumption-specific part:
    freq_diff <- max(0L, ceiling(n_na_surplus / length(unique_x)))
    if (is.na(freq_diff)) {
      freq_diff <- 0L
    }
    # END of the `max_unique = "known"`-assumption-specific part
  } else if (max_unique > length(unique_x)) {
    n_slots_empty_new_vals <- count_slots_empty_new_vals(n_na, freq_max)
  }

  freq_max <- freq_max_known + freq_diff
  n_final <- freq_max * length(unique_x)

  # Missings may include the hypothetical "supermodal" values (see below). This
  # is why, counterintuitively, even the mode among known values will have
  # missings counted towards it:
  x_out <- rep(unique_x, each = freq_max)
  freq_x <- table(x) + freq_diff
  is_missing <- logical()
  for (i in seq_along(unique_x)) {
    n_not_missing <- freq_x[i] - freq_diff
    is_missing <- c(is_missing, c(
      rep(FALSE, times = n_not_missing),
      rep(TRUE,  times = freq_max - n_not_missing)
    ))
  }

  # TODO: Parameterize the assumptions about missings! They are currently
  # hardcoded in a way that corresponds to `max_unique = "known"` in moder's
  # metadata functions. They also include the idea that the missings are as
  # evenly distributed among the known values as possible, so as to maximize the
  # number of hypothetical modes.

  # The vector of frequencies is a repeating sequence of a subsequence that goes
  # from 1 to the "supermode". The latter counts surplus missings (i.e., those
  # that remain after filling up all the empty slots) as hypothetically adding
  # to the mode, and it assumes the missings only represent known values and are
  # as evenly distributed among them as possible:
  freq_out_block1 <- seq_len(freq_max)
  freq_out <- rep(freq_out_block1, times = length(unique_x))

  # Fill in the empty slots:
  can_be_filled <- logical(n_final)
  for (i in freq_out_block1) {
    freq_indices <- which(freq_out == freq_out_block1[i])
    for (j in seq_along(freq_indices)) {
      if (n_na == 0L) {
        break
      } else if (is_missing[freq_indices[j]]) {
        can_be_filled[freq_indices[j]] <- TRUE
        n_na <- n_na - 1L
      }
    }
  }

  # "Supermodal" means that a missing observation is part of the "surplus" `NA`
  # observations that remain after filling up the empty slots, and that
  # hypothetically represent a known value:
  is_supermodal <- logical(n_final)
  is_supermodal[freq_out > freq_max_known] <- TRUE

  # The output is returned as a base R data frame, not as a tibble, because (a)
  # this package shouldn't have too many dependencies, (b) its columns are
  # highly controlled, and (c) it has an explicit `stringsAsFactors = FALSE`:
  data.frame(
    x = x_out, freq = freq_out, is_missing, can_be_filled, is_supermodal,
    stringsAsFactors = FALSE
  )
}
