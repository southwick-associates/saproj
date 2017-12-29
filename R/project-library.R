# functions for maintaining project package libraries

#' Create a snapshot (csv) of packages installed in project library
#' 
#' The snapshot improves project portability by storing details about installed
#' packages. The project library can be restored on a different machine using 
#' \code{\link{restore_library}}
#' @param proj_libpath character: The location of the project library 
#' (note: changing this argument is not recommended)
#' @family functions for maintaining project package libraries
#' @import dplyr
#' @export
#' @examples
#' saproj::snapshot_library()
snapshot_library <- function(proj_libpath = .libPaths()[1]) {
    
    # get comparison info on snapshot and package library
    comparison_outcome <- compare_library_snapshot(proj_libpath)
    
    # define conditions based on 5 possible message outcomes
    #   three alternatives below depending on comparison_outcome
    
    # a. throw errors if there is a problem
    if (names(comparison_outcome[1]) %in% c("conflicts", "library_behind")) {
        # print comparison_df to show differences
        comparison_outcome[["compare_df"]] %>%
            filter(in_library != in_snapshot) %>%
            print()
        stop(comparison_outcome[[1]])
        
    } else if (names(comparison_outcome[1]) == "neither") {
        stop(comparison_outcome[[1]])
        
    # b. send a polite message of snapshot is already up-to-date
    } else if (names(comparison_outcome[1]) == "same") {
        cat(paste("No snapshot was taken:", comparison_outcome[[1]], "\n\n"))
        print(comparison_outcome[[2]])
        
    # c. make a snapshot and show outcome
    } else {
        installed_packages <- comparison_outcome[["compare_df"]] %>%
            select(Package, Version)
        write.csv(installed_packages, file = "snapshot-library.csv", row.names = FALSE)
        
        # start message output
        cat(paste0("\nSnapshot taken from:\n", normalizePath(proj_libpath), "\n"))
        cat(paste0("\nSnapshot saved to:\n", normalizePath("snapshot-library.csv"), "\n"))
        
        # import and view snapshot
        cat("\nSnapshot Details:\n")
        utils::read.csv("snapshot-library.csv")
    }
}

#' Compare installed project packages to snapshot-library.csv
#' 
#' The results of the comparison inform the recommendation for actions to 
#' sync the installed library and it's snapshot.
#' @inheritParams snapshot_library
#' @family functions for maintaining project package libraries
#' @import dplyr
#' @export
#' @examples
#' compare_library_snapshot()
compare_library_snapshot <- function(proj_libpath = .libPaths()[1]) {
    
    # define 5 possible comparison outcomes
    outcomes <- list(
        neither = "No project-specific packages have been installed or recorded.",
        same = "Snapshot is up-to-date with project library.",
        snapshot_behind = "Packages missing from snapshot: run 'saproj::snapshot_library()' to update.",
        library_behind = "Packages missing from project library: run 'saproj::restore_library()' to update.",
        conflicts = paste0(
            "Your library snapshot conflicts with the installed project library: ",
            paste0("'", basename(proj_libpath), "'\n"),
            "To set a new library, run:\n",
            "1. saproj::update_project(proj_library = 'your-new-library-name')\n",
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
        snapshot_df <- utils::read.csv("snapshot-library.csv", stringsAsFactors = FALSE) %>%
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

#' Convenience function to look at a package library
#' 
#' This is a quick way of looking at packages installed, particularly useful for a 
#' project-specific library.
#' @param library_path character: path to package library
#' @family functions for maintaining project package libraries
#' @export
#' @examples
#' view_library()
view_library <- function(library_path = .libPaths()[1]) {
    
    # print library path
    # cat(paste0("\nPackage Library\n---------------\n", library_path, "\n\n\n"))
    cat(paste0(library_path, "\n\n"))
    
    # print installed packages (if any are installed)
    pkg <- utils::installed.packages(lib.loc = library_path)
    pkg <- data.frame(pkg)
    pkg <- pkg[c("Version")]
    cat("Packages Installed\n------------------\n")
    if (nrow(pkg) > 1) {
        print(pkg)
    } else {
        cat("[None]\n")
    }
    cat("\n")
}


# TODO - make this function
restore_library <- function() {}
