
# saproj: An R package for Southwick internal use

The basic intent of saproj is to provide a simple set of functions for  maintaining package libraries. I made this functionality because I wasn't satisfied with current offerings (packrat, checkpoint). saproj relies on the Southwick R Setup to provide a consistent set of default packages, and  adds a way of installing/documenting project package installations, so the user can have a customized (project-specific) set of additional libraries, and this can easily be reestablished on another machine. 

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
