
# saproj

A package that provides functions for  maintaining package libraries. Written because I wasn't satisfied with current offerings (packrat, checkpoint). It's intended to work with the [Southwick R-Setup](https://github.com/southwick-associates/R-setup).

## Deprecated

As of Feb 2020, I consider this approach deprecated. I recommend [package renv](https://rstudio.github.io/renv/index.html) and will add an `saproj::to_renv()` function to help migrate the necessary dependency specifications for use with renv (i.e., if you need to update an older workflow which used saproj for a new project).

## Installation

From the R console:

``` r
install.packages("remotes")
remotes::install_github("southwick-associates/saproj")
```

## Usage

See [`vignette("saproj-intro")`](vignettes/saproj-intro.md) for a guide.
