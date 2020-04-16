# functions for migrating to renv
# https://rstudio.github.io/renv/articles/renv.html

#' Archive the saproj settings in an "saproj_archive" folder
#' 
#' @param project_dir file path to project
#' @param files_to_move files to be archived
#' @family functions for migrating to renv
#' @export
archive_saproj <- function(
    project_dir, files_to_move = c(".Rprofile", "snapshot-library.csv")
) {
    if (file.exists(file.path(project_dir, "saproj_archive"))) {
        message("The saproj_archive folder already exists & no files were archived")
        return(invisible())
    }
    if (!any(file.exists(file.path(project_dir, files_to_move)))) {
        message("No files to migrate")
        return(invisible())
    }
    dir.create(file.path(project_dir, "saproj_archive"))
    files_exist <- file.exists(file.path(project_dir, files_to_move))
    files_to_move <- files_to_move[files_exist]
    for (i in files_to_move) {
        file.rename(
            file.path(project_dir, i),  
            file.path(project_dir, "saproj_archive", i)
        )
    }
}

#' Install previous saproj packages into renv library
#' 
#' To be called following \code{\link{archive_saproj}} and renv \code{\link[renv]{init}}.
#' If you get a cryptic error such as "Cannot open URL...", you can excluded the 
#' offending package-version with the exclude argument
#' 
#' @param snapshot path to snapshot-library.csv used by saproj
#' @param exclude optionally exclude specified packages. Useful if
#' \code{\link[renv]{install}} can't find the specified package and produces a cryptic error.
#' @family functions for migrating to renv
#' @export
install_saproj_packages <- function(
    snapshot = file.path("saproj_archive", "snapshot-library.csv"),
    exclude = c("sadash@1.0.4")
) {
    packages <- utils::read.csv(snapshot, stringsAsFactors = FALSE)
    packages$install <- paste0(packages$Package, "@", packages$Version)
    
    installed_df <- data.frame(
        utils::installed.packages(.libPaths()[1]),  
        stringsAsFactors = FALSE
    )
    installed_packages <- paste0(installed_df$Package, "@", installed_df$Version)
    already_installed <- packages$install %in% installed_packages
    packages_to_install <- packages$install[!already_installed]
    do_exclude <- packages_to_install %in% exclude
    packages_to_install <- packages_to_install[!do_exclude]
    
    for (i in packages_to_install) {
        tryCatch(
            renv::install(i), error = function(e) {
                print(paste("Package" , i, "couldn't be found, not installed."))
            }
        )
    }
}


