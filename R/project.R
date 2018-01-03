# functions for setting up projects

# for testing
# dir.create("test")
# setwd("test")

#' Populate a project with default directories and files
#' 
#' This also creates a space for project-specific packages (using .Rprofile)
#' @param project_library character: Name of project library to use for project-specific packages.
#' This is required in order to encourage reproducibility and portability.
#' @param inherit_library logical: If TRUE, will use an existing project library. 
#' The default is set to FALSE to avoid accidentally using an existing library.
#' @param r_version character: R version (e.g., "3.4.3") to use for the new project.
#' Defaults to currently loaded version.
#' @family functions for setting up projects
#' @export
#' @examples
#' saproj::new_project("new-project-name")
new_project <- function(project_library, inherit_library = FALSE,
                        r_version = paste(R.version$major, R.version$minor, sep = ".")) {
    
    # 1. prepare the project package library
    proj_libpath <- file.path(Sys.getenv("R_HOME"), "project-library", project_library)
    
    # stop if the project_library already exists
    if (!inherit_library & dir.exists(proj_libpath)) {
        stop(paste0(
            "The '", project_library, "' library already exists.\n",
            "Please set 'inherit_library = TRUE' if you want to use this library.\n\n",
            "Existing project libraries:\n",
            capture.output(list.dirs(dirname(proj_libpath), full.names = FALSE, recursive = FALSE))
        ))
    }
    
    # 2. make directories & template files
    dir.create("code")
    dir.create("data")
    dir.create("out")
    file.copy(system.file("misc", "README", package = "saproj"), "README.txt")
    file.copy(system.file("misc", ".Rprofile", package = "saproj"), ".Rprofile")
    
    # 3. Edit .Rprofile (to match r_version & proj_libpath)
    x <- readLines(".Rprofile")
    x[9] <- paste0("r_version <- '", r_version, "'")
    x[10] <- paste0("proj_libname <- '", project_library, "'")
    writeLines(x, ".Rprofile")
    
    # 4. print a message about new project created
    cat("Files and folders have been initialized for the project.\n")
    
    # 5. source .Rprofile to initialize project library
    source(".Rprofile")
}

#' Create a new code section (i.e., sub-folder)
#' 
#' This is a convenience function to start a new code 
#' section and also include corresponding sub-folders in "data/" and "out/".
#' @param section_title character: Name of sub-directory to create
#' @inheritParams base::dir.create
#' @family functions for setting up projects
#' @export
#' @examples
#' saproj::new_project()
#' saproj::new_section("1-first-section")
new_section <- function(section_title, recursive = TRUE) {
    dir.create(file.path("code", section_title), ...)
    dir.create(file.path("data", section_title), ...)
    dir.create(file.path("out", section_title), ...)
}

# helper function to look at existing project libraries
# probably not necessary since it is such a simple function
view_project_libraries <- function(proj_libpath = .libPaths()[1]) {
    list.dirs(dirname(proj_libpath), full.names = FALSE, recursive = FALSE)
}

# TODO - simple function for changing the 2 project parameters
# NULL values inherit the existing project_library & r_version
# might be worthwhile to make a view_project() that shows these
update_project <- function(project_library = NULL, r_version = NULL) {
    if (!is.null(r_version)) {
        # update the version
    }
    
    if (!is.null(project_library)) {
        # update the library
    }
}
