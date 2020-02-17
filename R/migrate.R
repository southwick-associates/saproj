# functions for migrating to renv
# https://rstudio.github.io/renv/articles/renv.html

#' Convert saproj project settings for use with package renv
#' 
#' This is intended for migration to a better dependency management system (renv).
#' The existing saproj settings will be archived in an "saproj" folder using 
#' \code{\link{archive_project}} in case they need to be retrieved in the future. 
#' An renv lockfile will be created for use with renv::restore().
#' 
#' @param update_r If TRUE, your default R version will be used for renv. If you 
#' update to a new version, there is a small possibility that breaking changes 
#' will be introduced.
#' @param update_packages If TRUE, will use new package versions instead of those
#' specified in snapshot-library.csv. The chances of breaking changes are greater
#' than those related to R version upates.
#' @inheritParams archive_project
#' @family functions for migrating to renv
#' @export
to_renv <- function(
    update_r = FALSE, update_packages = FALSE,
    files = c(".Rprofile", "snapshot-library.csv")
) {
    archive_project(files)
    if (!any(file.exists(file.path("saproj", files)))) {
        stop("No specified files are available to run migration", call. = FALSE)
    }
    # TODO: update renv lockfile with necessary specifications
    # - R version
    # - package info
    
    # need to also consider packages installed as part of R Setup
    # - dplyr, others?
    
    # probably include a message like "Success: use renv::restore() to build renv library"
}

#' Archive the saproj settings in an "saproj" folder
#' 
#' Intended to be called from \code{\link{to_renv}} as an intermediate step
#' which saves the existing saproj settings before removing the saproj-specified
#' .Rprofile and snapshot-library.csv
#' 
#' @param files files to be archived
#' @family functions for migrating to renv
#' @export
archive_project <- function(files = c(".Rprofile", "snapshot-library.csv")) {
    if (file.exists("saproj")) {
        message("The saproj folder already exists & no files were archived")
        return(invisible())
    }
    if (!any(file.exists(files))) {
        message("No files to migrate")
        return(invisible())
    }
    dir.create("saproj")
    for (i in files) {
        if (!file.exists(i)) next
        file.rename(i, file.path("saproj", i))
    }
}
