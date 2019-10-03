# package-level functions & documentation

#' @import dplyr stringr
#' @importFrom utils available.packages capture.output install.packages write.csv
NULL

# message to be displayed when package loads
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        "For an introduction to saproj:
> vignette('saproj-intro')")
}

if(getRversion() >= "2.15.1") utils::globalVariables(c(
    "Package", "Version", "Version_repo", "Version_snapshot", 
    "compare", "in_library", "in_snapshot"
))