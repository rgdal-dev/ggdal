
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggdal

<!-- badges: start -->

[![R-CMD-check](https://github.com/hypertidy/ggdal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hypertidy/ggdal/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ggdal is to …

This is really just an experiment, it’s very unclear to me if the gg-way
really fits the GDAL warper, but i’m going to try because a graphics
device should send messages to the GDAL library and not need a human
intermediary.

## Installation

You can install the development version of ggdal from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/ggdal")
```

# TODO

-   make useful
-   use the extent from gdal info, consider xmin, xmax, ymin, ymax aes?
-   look at crs, use of coord_sf or can we just ggplot2 more basic
-   probably, use coord_sf to set the grain and extent and crs …

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggdal)
## basic example code
```

## Code of Conduct

Please note that the ggdal project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
