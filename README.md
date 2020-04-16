
# saproj

A package that provides functions for  maintaining package libraries. Written because I wasn't satisfied with current offerings (packrat, checkpoint). It's intended to work with the [Southwick R-Setup](https://github.com/southwick-associates/R-setup).

## Deprecated

As of Feb 2020, I consider this approach deprecated. I recommend [package renv](https://rstudio.github.io/renv/index.html). 

### Migration

You can migrate the saproj settings for an existing project over to renv:

```r
# arhive the saproj tracking files
project_directory <- "E:/SA/Projects/Data-Dashboards/SC/2019-q4"
saproj::archive_saproj(project_directory)

# open .Rproj in the project_directory & setup packages in renv
renv::init(bare = TRUE)
renv::install("southwick-associates/saproj")
saproj::install_saproj_packages()
renv::snapshot()
```

## Installation

From the R console:

``` r
install.packages("remotes")
remotes::install_github("southwick-associates/saproj")
```

## Usage

See [`vignette("saproj-intro")`](vignettes/saproj-intro.md) for a guide.
