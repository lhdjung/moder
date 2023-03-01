
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
