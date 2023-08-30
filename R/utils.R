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
    "The `multiple` argument was renamed to `accept` in moder 0.3.0.",
    "`multiple` no longer has any effect. It will be removed in",
    "a future version."
  ))
}

