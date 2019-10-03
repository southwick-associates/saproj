
# saproj

The basic intent of saproj is to provide a simple set of functions for  maintaining package libraries. I made this functionality because I wasn't satisfied with current offerings (packrat, checkpoint). It's intended for Southwick internal use only.

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
