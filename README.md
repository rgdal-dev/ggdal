
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggdal

<!-- badges: start -->

[![R-CMD-check](https://github.com/hypertidy/ggdal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hypertidy/ggdal/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ggdal is to get background image data for ggplot2.

## Installation

You can install the development version of ggdal from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/ggdal")
```

# Example

Use ‘dsn = “osm”’ or “virtualearth”, or input custom (WIP).

Use any map projection. Currently examples only provided for use with sf
objects, WIP.

``` r
library(ggplot2)
library(ggdal)
data(iw)

ggplot() +
  annotation_gdal(dsn = "osm") +
  geom_sf(data = sf::st_transform(iw, "EPSG:3577"), fill = NA, col = "grey50")
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r


ggplot() +
  annotation_gdal(dsn = "virtualearth") +
  geom_sf(data = sfdct::antarctica, fill = NA, col = "grey50") + coord_sf(xlim = c(-6378137, 6378137), 
                                                                          ylim = c(-6378137, 6378137))
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
```

<img src="man/figures/README-example-2.png" width="100%" />

## Code of Conduct

Please note that the ggdal project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
