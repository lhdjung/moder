# moder (development version)

## Breaking changes

-   `mode_is_trivial()` was renamed to `is_uniform()`. This makes the interpretation more clear and appealing.

## New features

-   New functions `frequency_grid_df()` and `frequency_grid_plot()` to analyze and visualize possible frequency distributions given missing values.
-   New function `mode_df()` to help when other moder functions return `NA`.
-   New predicate functions `is_unimodal()` and `is_multimodal()` to check whether a vector has only one mode or multiple modes.

## Bugfixes

-   `mode_is_trivial()` no longer ignores `max_unique` if all known values are equally frequent.
-   `mode_count_range()` no longer returns a maximum of 0 for certain kinds of input.
-   `mode_count_range()`, `mode_possible_min()`, and `mode_possible_max()` no longer display a spurious warning if `x` is `NULL`.
-   `mode_possible_max()` no longer returns `NA` if the number of `NA`s in the input is equal to or higher than the number of "empty slots" (i.e., the hypothetical non-`NA` values that would have to be present in `x` in order for all of its unique values to be equally frequent). After all, it is possible for all unique values to be "filled up" by `NA`s so that they are themselves modes.
-   The tibble and ggplot2 packages are now imported, as are stats and utils. The former two are used by the new `frequency_grid_*()` functions, and the latter two should have been imported all along. (They used to be suggested but are needed at runtime.)

## Minor improvements

-   In `mode_possible_min()` and `mode_possible_max()`, the `multiple` argument was renamed to `accept` for greater consistency with `mode_first()` and other functions.

# moder 0.2.1

Patch for CRAN submission.

# moder 0.2.0

-   `mode_count()` and all other metadata functions now have a `max_unique` argument, allowing users to encode knowledge about missing values in the analysis.
-   `mode_single()` now has an `accept` argument for opting into a less strict check for a single mode, in analogy to `mode_first()`.
-   `mode_first()` had its `first_known` argument renamed to `accept` for consistency with `mode_single()`.
-   Further improvements to the documentation, including vignettes.

# moder 0.1.1

-   Fixed incongruencies in the documentation and in some warnings.
-   Established ⁠`R CMD check` with Github Actions.

# moder 0.1.0

-   New functions for modal metadata:
    -   `mode_is_trivial()`
    -   `mode_frequency()`
    -   `mode_count_range()`
    -   `mode_frequency_range()`
-   New vignettes on missing values, metadata, and performance.
-   Many bugfixes and other improvements.

# moder 0.0.1

-   Added a `NEWS.md` file to track changes to the package.
