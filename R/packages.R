# functions for working with R packages

#' Install Packages from Repositories or Local Files
#' 
#' This is a wrapper for \code{\link[utils]{install.packages}} that creates a record
#' of package installations. It always installs to the first argument of 
#' \code{\link[base]{.libPaths}}, which should correspond to either a project-specific 
#' or user library.
#' @inheritParams utils::install.packages
#' @param ... Arguments passed to \code{\link[utils]{install.packages}}
#' @family functions for working with R packages
#' @export
#' @examples
#' Install a CRAN package
#' saproj::install_packages("acs")
#' 
#' Install a local package
#' saproj::install_packages("pkg/salic_0.3.4.zip", repos = NULL, type = "win.binary")
install_packages <- function(pkgs, ...) {
   
    # install packages
    install.packages(pkgs = pkgs, lib = .libPaths()[1], ...)
    
    # define file location for package installation record
    record_filepath <- paste0(.libPaths()[1], "-install.R")
    if (!file.exists(record_filepath)) file.create(record_filepath)
    
    # get function call to write to record_file
    # (appending "saproj::" to beginning of line if it doesn't exist )
    sys_call <- as.character(list(sys.call()))
    if (substr(sys_call, 1, 8) != "saproj::") {
        sys_call <- paste0("saproj::", sys_call)
    }
    
    # write the new function call (if it doesn't already exist in record_file)
    record_file <- readLines(record_filepath)
    if (!(sys_call %in% record_file)) {
        write(sys_call, record_filepath, append = TRUE)
    }
    
}

# TODO - START HERE
view_packages <- function() {}

restore_packages <- function() {}
