
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Mode estimation in R

<!-- badges: start -->
<!-- badges: end -->

The moder package determines single or multiple **modes** (most frequent
values). By default, its functions check whether missing values make
this impossible, and return `NA` in this case. It has no dependencies.

Mode functions fill a gap in measures of central tendency in R. `mean()`
and `median()` are built into the standard library, but there is a lack
of go-to functions for calculating the mode. Earlier solutions don’t
handle missing values properly. Use moder for this!

## Installation

You can install the development version of moder like so:

``` r
remotes::install_github("lhdjung/moder")
```

## Get started

``` r
library(moder)
```

### Find the first mode with `mode_first()`

Everything is fine here:

``` r
mode_first(c(7, 8, 8, 9, 9, 9))
#> [1] 9
```

But what if some values are missing? Think about what it means to look
for the most frequent value in such a distribution. Maybe there are so
many missings that it’s impossible to tell which value is the most
frequent one:

``` r
mode_first(c(1, 1, 2, 2, 2, 2, NA, NA, NA, NA))
#> [1] NA
```

Ignore `NA`s using `na.rm = TRUE` if there is a strong rationale for it:

``` r
mode_first(c(1, 1, 2, 2, 2, 2, NA, NA, NA, NA), na.rm = TRUE)
#> [1] 2
```

This distribution is different:

``` r
mode_first(c(7, 7, 7, 7, 8, 8, NA))
#> [1] 7
```

Even if the `NA` stands in for `8`, there will only be three instances
of `8` but four instances of `7`. The mode is `7`, independent of the
true value behind `NA`.

### Find all modes with `mode_all()`

This function captures multiple modes:

``` r
mode_all(c("a", "a", "b", "b", "c", "d", "e"))
#> [1] "a" "b"
```

If some values are missing but there would be multiple modes when
ignoring `NA`s, `mode_all()` returns `NA`. That’s because missings can
easily create an imbalance between the equally-frequent known values:

``` r
mode_all(c(1, 1, 2, 2, NA))
#> [1] NA
```

If `NA` masks either `1` or `2`, that number is the (single) mode. As
before, if the mode depends on missing values, the function returns
`NA`.

Yet `na.rm = TRUE` makes the function ignore this:

``` r
mode_all(c(1, 1, 2, 2, NA), na.rm = TRUE)
#> [1] 1 2
```

### Find the single mode (or `NA`) with `mode_single()`

`mode_single()` is stricter than `mode_first()`: It returns `NA` if
there are multiple modes. Otherwise, it works the same way.

``` r
mode_single(c(3, 4, 4, 5, 5, 5))
#> [1] 5
mode_single(c("x", "x", "y", "y", "z"))
#> [1] NA
```

### Find possible modes

These minimal and maximal sets of modes are possible given the missing
value:

``` r
mode_possible_min(c("a", "a", "a", "b", "b", "c", NA))
#> [1] "a"
mode_possible_max(c("a", "a", "a", "b", "b", "c", NA))
#> [1] "a" "b"
```
