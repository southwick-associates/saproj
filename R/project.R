# functions for setting up projects

#' Create a new sub-directory for R code, data, and output
#' @param title character: Name of sub-directory to create
#' @param projdir character: Subdirectory to place folders/files, intended for
#' testing. If NULL, the working directory will be used.
#' @param ... Additional arguments passed to \code{\link[base]{dir.create}}
#' @family functions for setting up projects
#' @export
#' @examples
#' saproj::new_project(projdir = "test-dir")
#' saproj::new_section("1-first-section", projdir = "test-dir")
new_section <- function(title, projdir = NULL, ...) {
    dir_create(file.path("code", title), projdir, ...)
    dir_create(file.path("data", title), projdir, ...)
    dir_create(file.path("doc", title), projdir, ...)
    dir_create(file.path("out", title), projdir, ...)
}

# helper function to look at existing project libraries
view_project_libraries <- function(proj_libpath = .libPaths()[1]) {
    list.dirs(dirname(proj_libpath), full.names = FALSE, recursive = FALSE)
}

# TODO 
# - Maybe drop the dat, dat2 options
# - parameters: proj_library, inherit_proj_library, R_version (maybe just use current)
# - also include .Rprofile

#' Populate a project with default directories and files
#' @param proj_library character: Name of project library
#' @param inherit_library logical: If TRUE, will use an existing project library
#' @inheritParams new_section
#' @family functions for setting up projects
#' @export
#' @examples
#' saproj::new_project("some-project-name", projdir = "test-dir")
new_project <- function(proj_library, inherit_library = FALSE, projdir = NULL) {
    
    # 1. prepare the project package library
    proj_libpath <- file.path(Sys.getenv("R_HOME"), "project-library", proj_library)
    
    # stop if the proj_library already exists
    if (!inherit_library) {
        stop(paste0(
            "The '", proj_library, "' library already exists.\n",
            "Please set 'inherit_library = TRUE' if you want to use this library.\n\n",
            "Existing project libraries:\n",
            capture.output(list.dirs(dirname(proj_libpath), full.names = FALSE, recursive = FALSE))
        ))
    }
    dir.create() # make library (this might not be needed here since the Rprofile will create it)
    
    # 2. build directories
    dir_create("code", projdir)
    dir_create("data", projdir)
    dir_create("out", projdir)
    
    # 3. template files
    file_copy(system.file("misc", "README", package = "saproj"),
              "README.txt", projdir)

    file_copy(system.file("misc", ".Rprofile", package = "saproj"),
              "README.txt", projdir)
    
   
    
    # note - probably want the tell user to restart
    # the Rprofile needs to be loaded to take effect...
    # maybe we can just run the Rprofile at the end
}

# TODO - simple function for changing the 2 project parameters
# NULL values inherit the existing proj_library & r_version
# might be worthwhile to make a view_project() that shows these
update_project <- function(proj_library = NULL, r_version = NULL) {
    if (!is.null(r_version)) {
        # update the version
    }
    
    if (!is.null(proj_library)) {
        # update the library
    }
}
