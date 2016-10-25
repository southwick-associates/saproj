# miscellaneous functions

# message to be displayed when package loads
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        "For an introduction to tablr:
> vignette('saproj-intro')")
}
