# Test vectors:
x1 <- c(7, 8, 8, 9, 9, 9)
x2 <- c(1, 1, 2, 2, 2, 2, NA, NA, NA, NA)
x3 <- c(7, 7, 7, 7, 8, 8, NA)
x4 <- c("a", "a", "b", "b", "c", "d", "e")
x5 <- c(1, 1, 2, 2, NA)
x6 <- c(3, 4, 4, 5, 5, 5)
x7 <- c("x", "y", "y", "z", "z")
x8 <- c(1, 1, 2, NA)
x9 <- c(2, 1, 1, NA)
x10 <- c(1, 1, NA)
x11 <- c(1, NA)
x12 <- c("a", "a", "a", "b", "b", "c", "d", "e", NA)
x13 <- c(NA, 1)
x14 <- c(1, 1, 2, 2, 3, NA)
x15 <- c(1, NA, NA)
x16 <- c(1, 1, 1, 2, 2, 3, 3, NA)
x17 <- c(1, 1, 2, 2, NA, NA)
x18 <- c(1, 1, 1, 2, 2, 3, 3, NA, NA, NA)

utils::globalVariables(c(
  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17,
  "freq"
))

# # Test interactively:
# cat(paste0("mode_count_range(x", 1:17, ")\n"))



# Helper functions (not exported) -----------------------------------------

# Called within `mode_all()`:
decide_mode_na <- function(x, unique_x, mode1) {
  if (length(unique_x) == 1L) {
    if (length(x[is.na(x)]) < length(x) / 2L) {
      return(mode1)
    } else {
      return(x[NA_integer_])
    }
  }
  ix2 <- x[x != mode1 & !is.na(x)]
  frequency2 <- vapply(ix2, function(x) length(ix2[ix2 == x]), 1L)
  mode2 <- ix2[which.max(frequency2)]
  n_na <- length(x[is.na(x)])
  x <- x[!is.na(x)]
  n_mode1 <- length(x[x == mode1])
  n_mode2_na <- length(x[x == mode2]) + n_na
  if (n_mode1 > n_mode2_na) {
    mode1
  } else {
    x[NA_integer_]
  }
}

# ONLY FOR VECTORS WITHOUT MISSING VALUES! Count the occurrences that all
# non-modal values together lack compared to the mode. These "empty slots" would
# have to be filled by `NA`s in a hypothetical scenario in order for the
# respective values to be modes:
count_slots_empty <- function(x) {
  frequency_max <- length(x[x %in% mode_first_if_no_na(x)])
  n_slots_all <- length(unique(x)) * frequency_max
  n_slots_all - length(x)
}


# ONLY FOR VECTORS WITHOUT MISSING VALUES! The number of empty slots within the
# part of the grid added by `NA`s -- hypothetical values beyond the known ones:
count_slots_empty_new_vals <- function(n_na, frequency_max) {
  n_new_vals <- ceiling(n_na / frequency_max)
  n_slots_all <- n_new_vals * frequency_max
  n_slots_all - n_na
}


# ONLY FOR VECTORS WITHOUT MISSING VALUES, AND WITHOUT ARGUMENTS LIKE
# `numeric(0)`! A faster version of `mode_all()`:
mode_all_if_no_na <- function(x) {
  frequency1 <- vapply(x, function(y) length(x[x == y]), 1L)
  unique(x[frequency1 == max(frequency1)])
}


# ONLY FOR VECTORS WITHOUT MISSING VALUES, AND WITHOUT ARGUMENTS LIKE
# `numeric(0)`! A faster version of `mode_first()`:
mode_first_if_no_na <- function(x) {
  frequency1 <- vapply(x, function(y) length(x[x == y]), 1L)
  x[which.max(frequency1)]
}


print_example_x <- function() {
  cat(paste0("x", 1:17, "\n"))
}


check_factor_max_unique <- function(x, n_na, fn_name) {
  if (is.factor(x)) {
    msg_warn <- paste0("In `", fn_name, "()`, `max_unique` should be \"known\"")
    msg_warn <- paste(msg_warn, "if `x` is a factor (the presumption is")
    msg_warn <- paste(msg_warn, "that all factor levels are known).")
    # Calculate the modal frequency, which can put the issue in perspective if
    # it's high enough:
    if (n_na < max(vapply(x, function(y) length(x[x == y]), 1L))) {
      msg_warn <- paste(msg_warn, "\nIt won't matter in this particular case")
      msg_warn <- paste(msg_warn, "because there are not enough `NA`s to")
      msg_warn <- paste(msg_warn, "form any new modes.")
    }
    warning(msg_warn)
  }
}


# Call this helper in each function that has a `max_unique` argument, but only
# if the user overrode the default of `max_unique = NULL`:
handle_max_unique_input <- function(x, max_unique, n_unique_x, n_na, fn_name) {
  if (is.null(max_unique)) {
    check_factor_max_unique(x, n_na, fn_name)
    return(NULL)
  } else if (max_unique == "known") {
    return(n_unique_x)
  } else {
    check_factor_max_unique(x, n_na, fn_name)
  }
  # Throw an error if `max_unique` is misspecified. Else, return `max_unique` so
  # that it will be reassigned on the level of the caller function:
  if (is.character(max_unique)) {
    msg_error <- paste0("In `", fn_name, "()`, `max_unique` is \"", max_unique)
    msg_error <- paste0(msg_error, "\". If it's a string, `max_unique` should")
    msg_error <- paste(msg_error, "be \"known\".")
    stop(msg_error)
  } else if (max_unique < n_unique_x) {
    msg_error <- paste0("In `", fn_name, "()`, ", "`max_unique` is ")
    msg_error <- paste(msg_error, max_unique, "but there are", n_unique_x)
    msg_error <- paste(msg_error, "unique values in `x`.")
    stop(msg_error)
  } else {
    return(max_unique)
  }
}


# Warning after deprecation check in `mode_possible_min()` and
# `mode_possible_max()`:
mode_possible_warn_multiple <- function() {
  warning(paste(
    "The `multiple` argument in `mode_possible_min()` and",
    "`mode_possible_max()` was renamed to `accept` in moder 0.3.0.",
    "`multiple` no longer has any effect. It will be removed in",
    "a future version."
  ))
}


# Adapted with modifications from the examples of the `?integer` documentation.
# For each element of `x`, this vectorized helper checks whether it is
# *conceptually* an integer (but not necessarily by object type). If `x` is not
# numeric, a vector of `FALSE` values with the same length as `x` is returned
# because every single element is not a (whole) number.
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  if (is.numeric(x)) {
    abs(x - round(x)) < tolerance
  } else {
    logical(length(x))
  }
}


# This helper handles the `na.rm.amount` argument in the proper mode functions:
# `mode_first()`, `mode_all()`, and `mode_single()`. It removes a number of
# missing values from `x` equal to `na.rm.amount`, then returns `x`. Notes:
# -- The specification of `na.rm.from` should be checked by the calling
# function, like `na.rm.from <- match.arg(na.rm.from)`.
# -- For efficiency, `decrease_na_amount()` should only be called under very
# specific conditions, as in `mode_first()` etc.
decrease_na_amount <- function(x, na.rm, na.rm.amount,
                               na.rm.from = c("first")) {
  # Check for misspecifications of the calling function's arguments:
  if (na.rm) {
    stop(paste(
      "Conflicting instructions: `na.rm` removes all missing values,",
      "`na.rm.amount` only removes some number of them."
    ))
  }
  amount_is_wrong <- length(na.rm.amount) != 1L ||
    !is_whole_number(na.rm.amount) ||
    na.rm.amount < 0
  if (amount_is_wrong) {
    stop("`na.rm.amount` must be a single whole, non-negative number.")
  }
  # Vectors without any `NA`s have nothing to remove, so they should be returned
  # as they are:
  if (!anyNA(x)) {
    return(x)
  }
  # Record the indices of missing values in `x`:
  x_na <- which(is.na(x))
  # Subtract `na.rm.amount` from the number of missing values:
  nna <- length(x_na) - na.rm.amount
  # In case no `NA`s remain after this subtraction, all of them need to be
  # removed from `x`:
  if (nna <= 0L) {
    return(x[-x_na])
  }
  # Target the indices of those missings that should be ignored:
  x_na_but_ignored <- switch(
    na.rm.from,
    "first"  = utils::head(x_na, length(x_na) - nna),
    "last"   = utils::tail(x_na, length(x_na) - nna),
    "random" = sample(x_na, length(x_na) - nna)
  )
  # Return `x`, excluding those values:
  x[-x_na_but_ignored]
}
