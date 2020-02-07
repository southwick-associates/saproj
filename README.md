
# saproj

The basic intent of package saproj is to provide a simple set of functions for  maintaining package libraries. I made this functionality because I wasn't satisfied with current offerings (packrat, checkpoint). It's intended to work with the [Southwick R-Setup](https://github.com/southwick-associates/R-setup) and was built for Southwick internal use only.

*Feb 2020 Note: A new package ([renv](https://rstudio.github.io/renv/index.html)) appears to address the issues I've had with previous dependency management solutions, and it may serve as a good replacement for saproj. It appears to have a similar philosophy to saproj and it's almost certainly more robust (having been developed by Rstudio folks).*

## Installation

Install saproj using the devtools package:

``` r
# install dependencies
install.packages(c("dplyr", "stringr"))

# install saproj
devtools::install_github("southwick-associates/saproj")
```

## Usage

See `vignette("saproj-intro")` for a guide. The basic analyst workflow:

- Initialize a project with `new_project()`.
- Record library changes with `snapshot_library()`, which the user will be prompted to do at R project startup.
- Reestablish on a new machine with `restore_library()`.
