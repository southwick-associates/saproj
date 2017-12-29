# functions for maintaining project package libraries

# TODO - Modify this function to work with compare_library_snapshot()

#' Create a snapshot (csv) of packages installed in project library
#' 
#' The snapshot improves project portability by storing details about installed
#' packages. The project library can then be restored using \code{\link{restore_library}}
snapshot_library <- function(proj_libpath = .libPaths()[1], resolve_conflicts = FALSE) {
    
    # get current library info
    # stop with error if there are no packages in target library
    installed_packages <- view_library(proj_libpath)
    
    # get current snapshot info (if it exists)
    
    # stop with wrror if the snapshot is up-to-date
    
    # make snapshot
    installed_packages <- data.frame(utils::installed.packages(proj_libpath))
    installed_packages <- installed_packages[c("Package", "Version")]
    write.csv(installed_packages, file = "snapshot-library.csv", row.names = FALSE)
    
    # start message output
    cat(paste0("\nSnapshot taken from:\n", normalizePath(proj_libpath), "\n"))
    cat(paste0("\nSnapshot saved to:\n", normalizePath("snapshot-library.csv"), "\n"))
    
    # import and view snapshot
    cat("\nSnapshot Details:\n")
    read.csv("snapshot-library.csv")
}

# TODO - probably delete this
# helper function to check project library and compare to snapshot
view_library <- function(proj_libpath = .libPaths()[1]) {
    
    # get installed packages
    installed_packages <- utils::installed.packages(proj_libpath)
    
    # stop with error if there are no packages in target library
    # maybe only do this in snapshot_library()
    if (length(installed_packages) == 0) {
        stop(paste("No packages are installed in", proj_libpath), call. = TRUE)
    } 
    
    # convert to useful data frame
    installed_packages <- data.frame(installed_packages)
    installed_packages <- installed_packages[c("Package", "Version")]
    installed_packages
}

# TODO - finish checking & make sure the @import works
#' Compare installed project packages to snapshot-library.csv
#' 
#' The results of the comparison inform the recommendation for actions to 
#' sync the installed library and it's snapshot.
#' @param proj_libpath character The location of the project library (should use default)
#' @family functions for maintaining project package libraries
#' @import dplyr
#' @export
#' @examples
#' compare_library_snapshot()
compare_library_snapshot <- function(proj_libpath = .libPaths()[1]) {
    
    # define 5 possible comparison outcomes
    outcomes <- list(
        neither = "No packages have been installed or recorded for this project.",
        same = "Snapshot is up-to-date with project library.",
        snapshot_behind = "Packages missing from snapshot: run saproj::snapshot_library() to update.",
        library_behind = "Packages missing from project library: run saproj::restore_library() to update.",
        conflicts = paste(
            "Warning: Your library snapshot conflicts with the installed project library.\n",
            "To set a new library run\n",
            "1. saproj::update_project(proj_library = 'your-new-library-name') &\n",
            "2. saproj::restore_library()"
        )
    )
 
    # get details about installed packages
    library_df <- data.frame(utils::installed.packages(proj_libpath), stringsAsFactors = FALSE) %>%
        select(Package, Version) %>%
        mutate(in_library = TRUE)
    if (nrow(library_df) > 0) has_library = TRUE else has_library = FALSE
    
    # get current snapshot
    if (file.exists("snapshot-library.csv")) {
        has_snapshot = TRUE
        snapshot_df <- read.csv("snapshot-library.csv", stringsAsFactors = FALSE) %>%
            mutate(in_snapshot = TRUE)
    } else {
        has_snapshot = FALSE
    }
    
    # produce comparison_outcome (list) that has two elements
    # 1. selected outcome message from outcomes list above
    # 2. data frame (compare_df) which shows the package-by-package comparison details
    
    # the data frame needs to be created by joining library_df & snapshot_df
    # the join used depends on 4 possible existence comparisons for has_library & has_snapshot
    if (!has_library & !has_snapshot) {
        # no comparison data frame is needed in this case since the outcome is obvious 
        # (i.e., no packages installed or recorded)
        comparison_outcome <- outcomes[names(outcomes) == "neither"]
        
    # the comparison outcomes for these depend on compare_df
    } else if (has_library & has_snapshot) {
        compare_df <- full_join(library_df, snapshot_df, by = c("Package", "Version")) %>%
            mutate(
                in_library = ifelse(is.na(in_library), FALSE, TRUE),
                in_snapshot = ifelse(is.na(in_snapshot), FALSE, TRUE)
            ) %>%
            arrange(Package, Version)
    } else if (has_library & !has_snapshot) {
        compare_df <- library_df %>% mutate(in_snapshot = FALSE)
    } else {
        compare_df <- snapshot_df %>% mutate(in_library = FALSE)
    }
    
    # look at compare_df to finish comparison
    if (has_library | has_snapshot) {
        diff <- filter(compare_df, in_library != in_snapshot)
        if (nrow(diff) == 0) {
            comparison_outcome <- outcomes[names(outcomes) == "same"]
        } else if (all(diff$in_snapshot)) {
            comparison_outcome <- outcomes[names(outcomes) == "library_behind"]
        } else if (all(diff$in_library)) {
            comparison_outcome <- outcomes[names(outcomes) == "snapshot_behind"]
        } else {
            comparison_outcome <- outcomes[names(outcomes) == "conflicts"]
        }
        comparison_outcome[["compare_df"]] <- compare_df
    }
    comparison_outcome
}

restore_library <- function() {}
