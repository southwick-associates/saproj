# functions for setting up projects

# for testing
# dir.create("test")
# setwd("test")


#' Get full library path (not exported - internal saproj only)
#' 
#' This is a helper function for use in new_project(), setup_project() and 
#' update_project(). It gets the full library path and stops function 
#' execution if the project_library exists (unless 'inherit_library = TRUE'). 
#' This is intended to avoid a situation where the user accidently inherits 
#' an existing library.
#' @inheritParams new_project
#' @family internal helper functions
#' @return Returns the full path to project_library
#' @keywords internal
#' @export
#' @examples
#' # get_library_path("example-existing-project")
get_library_path <- function(project_library, inherit_library = FALSE) {
    
    proj_libpath <- file.path(Sys.getenv("R_HOME"), "project-library", project_library)
    
    # error condition if the project_library already exists
    if (!inherit_library & dir.exists(proj_libpath)) {
        stop(paste0(
            "The '", project_library, "' library already exists.\n",
            "Please set 'inherit_library = TRUE' if you want to use this library.\n\n",
            "Existing project libraries:\n",
            capture.output(
                list.dirs(dirname(proj_libpath), full.names = FALSE, recursive = FALSE)
            )
        ), call. = FALSE)
    }
    proj_libpath
}


#' Set project parameters for an in-progress analysis
#' 
#' This sets up the reproducibility/portability by setting (1) R version
#' and (2) project library. This information will be stored in '.Rprofile'.
#' @param project_library character: Name of project library to use for project packages.
#' This is required in order to encourage reproducibility and portability.
#' @param inherit_library logical: If TRUE, will use an existing project library. 
#' The default is set to FALSE to avoid accidentally using an existing library.
#' @param r_version character: R version (e.g., "3.4.3") to use for the project.
#' Defaults to currently loaded version.
#' @family functions for setting up projects
#' @export
#' @examples
#' # saproj::setup_project("new-project-name")
setup_project <- function(
    project_library, inherit_library = FALSE, 
    r_version = paste(R.version$major, R.version$minor, sep = ".")
) {
    
    # error condition: stop if an .Rprofile exists
    # to avoid potential accidental mishaps by user
    if (file.exists(".Rprofile")) {
        stop("There is already an '.Rprofile' in this folder.\n",
             "Make sure it isn't needed (and then remove it) before running this function.",
             call. = FALSE)
    }
    
    # 1. prepare the project package library
    proj_libpath <- get_library_path(project_library, inherit_library)
    
    # 2. make template files
    if (!file.exists("README.txt")) {
        file.copy(system.file("misc", "README.txt", package = "saproj"), "README.txt")
    }
    
    # edit .Rprofile (to match r_version & proj_libpath)
    x <- readLines(system.file("misc", ".Rprofile", package = "saproj"))
    x[9] <- paste0("r_version <- '", r_version, "'")
    x[10] <- paste0("proj_libname <- '", project_library, "'")
    writeLines(x, ".Rprofile")
    
    # 4. print a message about project setup
    message(
        "\nThis project has been setup to use a package library:\n",
        proj_libpath, "\n"
    )
    
    # 5. source .Rprofile to initialize project library
    source(".Rprofile")
}

#' Populate a project with default directories and files
#' 
#' This is a convenience function, intended for use with a new analysis. 
#' It calls \code{\link{setup_project}}, and also makes a few template folders.
#' @inheritParams setup_project
#' @family functions for setting up projects
#' @export
#' @examples
#' # saproj::new_project("new-project-name")
new_project <- function(
    project_library, inherit_library = FALSE, 
    r_version = paste(R.version$major, R.version$minor, sep = ".")
) {
    
    # error condition: stop if this isn't an empty directory
    # assuming only a "xxx.Rproj" exists
    files <- list.files()
    files <- files[!grepl("\\.Rproj$", files)] # to exclude any .Rproj files
    if (length(files) > 0) {
        stop("This isn't an empty folder.\n",
             "Use setup_project() for an in-progress analysis.",
             call. = FALSE)
    }
    
    # 1. setup
    setup_project(project_library, inherit_library, r_version)
    
    # 2. make folders
    dir.create("code")
    dir.create("data")
    dir.create("out")
}


#' Create a new code section (i.e., sub-folder)
#' 
#' This is a convenience function to start a new  folder in "code/" 
#' and also include corresponding sub-folders in "data/" and "out/".
#' @param section_title character: Name of sub-directory to create
#' @inheritParams base::dir.create
#' @family functions for setting up projects
#' @export
#' @examples
#' # saproj::new_project("new-project-name")
#' # saproj::new_section("1-first-section")
new_section <- function(section_title, recursive = TRUE) {
    dir.create(file.path("code", section_title), recursive = recursive)
    dir.create(file.path("data", section_title), recursive = recursive)
    dir.create(file.path("out", section_title), recursive = recursive)
}


#' Update project R version or package library
#' 
#' This allows the 2 basic reproducibility parameters to be updated. This
#' would be useful (for example) if you want to adapt code/data of an 
#' existing project for a new project.
#' @param project_library character: New project library to use. Defaults
#' to no change.
#' @param r_version character: New R version to use (e.g., "3.4.3"). Defaults
#' to no change.
#' @inheritParams setup_project
#' @family functions for setting up projects
#' @export
#' @examples
#' # saproj::update_project(project_library = "new-project-name")
#' # saproj::update_project(r_version = "3.4.3")
update_project <- function(project_library = NULL, r_version = NULL,
                           inherit_library = FALSE) {
    
    # stop with error if no changes have been specified
    if (is.null(project_library) & is.null(r_version)) {
        stop("You haven't selected any project parameters to update.", call. = FALSE)
    }
    
    # initialize a message for printing at the end
    msg <- c()
    
    # 1. Get current .Rprofile
    if (!file.exists(".Rprofile")) {
        stop("No '.Rprofile' exists in the working directory\n",
             "(i.e., no project parameters to update).", call. = FALSE)
    }
    x <- readLines(".Rprofile")
    
    
    # 2. Edit .Rprofile (to match r_version & proj_libpath)
    if (!is.null(project_library)) {
        # this is used for the inherit_library error condition
        proj_libpath <- get_library_path(project_library, inherit_library)
        
        # edit .Rprofile
        x[10] <- paste0("proj_libname <- '", project_library, "'")
        
        # update message
        old_project_library <- basename(.libPaths()[1])
        msg <- c(msg, paste0("- project_library: '", old_project_library, 
                             "' changed to '", project_library, "'.\n"))
        
    }
    
    if (!is.null(r_version)) {
        # update R version
        x[9] <- paste0("r_version <- '", r_version, "'")

        # update message
        old_r_version <- paste(R.version$major, R.version$minor, sep = ".")
        msg <- c(msg, paste0("- r_version: '", old_r_version, 
                             "' changed to '", r_version, "'.\n"))
    }
    
    # 3. Update .Rprofile with specified changes
    writeLines(x, ".Rprofile")
    
    # 4. Print a message about project update
    message("\nProject Parameters have been updated:\n", msg, "\n")
    
    # 5. source .Rprofile to initialize any changes to project library
    source(".Rprofile")
}


#' List project libraries stored on this computer
#' 
#' This is intended to help out if you are wondering what to name a new project
#' (i.e., the 'project_library' parameter in \code{\link{new_project}}, etc.).
#' @param list_packages logical: If TRUE, includes a list of packages installed
#' in each project library.
#' @param all_versions logical: If TRUE, include all versions of R installed on 
#' this machine. Defaults to using only the currently loaded version.
#' @family functions for setting up projects
#' @import dplyr
#' @export
#' @examples
#' # view_projects()
#' # view_projects(list_packages = TRUE)
#' # view_projects(list_packages = TRUE, all_versions = TRUE)
view_projects <- function(
    list_packages = FALSE,
    all_versions = FALSE
) {
    # get a list of available R versions
    R_path <- Sys.getenv("R_HOME") %>% dirname()
    if (all_versions) {
        versions <- list.dirs(R_path, recursive = FALSE) %>% basename()
    } else {
        versions <- basename(Sys.getenv("R_HOME"))
    }
    
    # make a data frame with 2 variables, R.Version, Project.Library
    # the loop allows stacking over all versions
    libs <- list()          # project library names
    lib_df <- list()        # data frame with Version & Library
    for (i in versions) {
        libs[[i]] <- file.path(R_path, i, "project-library") %>%
            list.dirs(full.names = TRUE, recursive = FALSE)
        lib_df[[i]] <- data.frame(
            R.Version = i, Project.Library = basename(libs[[i]]),
            stringsAsFactors = FALSE)
    }
    lib_df <- bind_rows(lib_df)
    
    # get info about installed packages (if applicable)
    if (list_packages) {
        libs <- unlist(libs)
        pkgs <- c()
        for (i in seq_along(libs)) {
            pkg <- list.dirs(libs[i], recursive = FALSE, full.names = FALSE)
            pkgs[i] <- paste0("(", paste(pkg, collapse = ", "), ")")
        }
        lib_df$Installed.Packages <- pkgs
    }
    
    # print a message with project library details
    message("\nProject library Info:\n",  "---------------------")
    print(lib_df, right = FALSE, row.names = FALSE)
}
