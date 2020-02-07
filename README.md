
# saproj

A package that provides functions for  maintaining package libraries. Written because I wasn't satisfied with current offerings (packrat, checkpoint). It's intended to work with the [Southwick R-Setup](https://github.com/southwick-associates/R-setup).

*Feb 2020 Note: A new package ([renv](https://rstudio.github.io/renv/index.html)) appears to address the issues I've had with previous dependency management solutions, and it may serve as a good replacement for saproj. It appears to have a similar philosophy and it's almost certainly more robust (having been developed by Rstudio folks).*

## Usage

See [`vignette("saproj-intro")`](vignettes/saproj-intro.md) for a guide. The basic analyst workflow:

- Initialize a project with `new_project()`.
- Record library changes with `snapshot_library()`, which the user will be prompted to do at R project startup.
- Reestablish on a new machine with `restore_library()`.

Development planning/reference: [Development.md](Development.md) 

## Installation

Saproj should be available in the [Southwick R-Setup](https://github.com/southwick-associates/R-setup). The most recent version can be installed from github:

``` r
install.packages(c("dplyr", "stringr")) # dependencies
install.packages("devtools")
devtools::install_github("southwick-associates/saproj")
```
