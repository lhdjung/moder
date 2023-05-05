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
