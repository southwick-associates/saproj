# functions for working with R packages

#' Install Packages from Repositories or Local Files
#' 
#' This is a wrapper for \code{\link[utils]{install.packages}} that creates a record
#' of package installations. It always installs to the first element of 
#' \code{\link[base]{.libPaths}}, which should correspond to either a project-specific 
#' or user library. You can check this by running .libPaths() in the console.
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
    
    ### 1. install package(s)
    
    # setup error condition(s) to stop function execution
    if (missing(pkgs)) stop("The 'pkgs' argument must be supplied", call. = TRUE)
    
    # install packages
    utils::install.packages(pkgs = pkgs, lib = .libPaths()[1], ...)
    
    # START HERE!!!
    # TODO - Need to insure this only runs if install.packages() is successful
    # maybe tryCatch() with stop on warning is an approach, need to investigate
    
    ### 2. modify the installation history in a record file
    # the following should only run if utils::install.packages() completes successfully
    
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

#' View packages installed in library
#' 
#' This is a quick way of looking at packages installed, particularly useful for a 
#' project-specific library.
#' @param library_path character: path to package library
#' @family functions for working with R packages
#' @export
#' @examples
#' view_packages()
view_packages <- function(library_path = .libPaths()[1]) {
    
    # print library path
    # cat(paste0("\nPackage Library\n---------------\n", library_path, "\n\n\n"))
    cat(paste0(library_path, "\n\n"))
    
    # print contencts of record file (if it exists)
    record_filepath <- paste0(library_path, "-install.R")
        cat("--- Installation Record ---\n")
    if (file.exists(record_filepath)) {
        # doesn't work to store this in an object
        writeLines(readLines(record_filepath))
    } else {
        cat("[None]\n")
    }
    
    # print installed packages (if any are installed)
    pkg <- utils::installed.packages(lib.loc = library_path)
    pkg <- data.frame(pkg)
    pkg <- pkg[c("Version")]
    cat("\n--- Packages Installed ---\n")
    if (nrow(pkg) > 1) {
        print(pkg)
    } else {
        cat("[None]\n")
    }
    cat("\n")
}

# TODO - probably just running [project]-install.R
# maybe only useful for the wrap() > restore() workflow, so maybe not needed
restore_packages <- function() {}
