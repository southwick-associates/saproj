# package-level functions & documentation

# message to be displayed when package loads
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        "For an introduction to saproj:
> vignette('saproj-intro')")
}
