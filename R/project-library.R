# functions for maintaining project package libraries

#' Convenience function to compare installed project packages to snapshot-library.csv
#' 
#' The results of the comparison inform the recommendation for actions in
#' syncing the package library and it's snapshot.
#' @inheritParams snapshot_library
#' @family functions for maintaining project package libraries
#' @import dplyr
#' @export
#' @examples
#' compare_library_snapshot()
compare_library_snapshot <- function(proj_libpath = .libPaths()[1]) {
    
    # define 5 possible comparison outcomes & corresponding messages to display
    outcomes <- list(
        neither = "No project-specific packages have been installed or recorded.",
        same = "Snapshot is up-to-date with project library.",
        snapshot_behind = paste0(
            "Packages missing from snapshot:\n",
            "- run 'saproj::snapshot_library()' to update."
        ),
        library_behind = paste0(
            "Snapshotted packages missing from project library. Either:\n", 
            "a. update library with 'saproj::restore_library()', or\n",
            "b. reset snapsot with 'saproj::snapshot_library(replace_snapshot = TRUE)'"
        ),
        conflicts = paste0(
            "The snapshot conflicts with the project library ('",
            basename(proj_libpath), "'). Either:\n",
            "a. reset snapshot with 'saproj::snapshot_library(replace_snapshot = TRUE)', or\n",
            "b. set a new library with:\n",
            "   1. saproj::update_project(proj_library = 'your-new-library-name')\n",
            "   2. saproj::restore_library()"
        )
    )
    
    # get details about installed packages
    library_df <- data.frame(utils::installed.packages(proj_libpath), stringsAsFactors = FALSE) %>%
        select(Package, Version) %>%
        mutate(in_library = TRUE)
    if (nrow(library_df) > 0) has_library = TRUE else has_library = FALSE
    
    # get current snapshot (if available)
    if (file.exists("snapshot-library.csv")) has_snapshot = TRUE else has_snapshot = FALSE
    if (has_snapshot) {
        snapshot_df <- utils::read.csv("snapshot-library.csv", stringsAsFactors = FALSE) %>%
            mutate(in_snapshot = TRUE)
    }
    
    # produce comparison_outcome (list) that has two elements
    # 1. selected outcome message from outcomes list above
    # 2. data frame (compare_df) which shows the package-by-package comparison details
    
    # the data frame needs to be created by joining library_df & snapshot_df
    # the join used depends on 4 possible existence comparisons for has_library & has_snapshot
    if (!has_library & !has_snapshot) {
        # no comparison data frame is needed since no packages are installed or recorded
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
    
    # look at compare_df differences to finish comparison
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

# A helper function for use in snapshot_library() and restore_library()
# It is used to throw an error based on the value returned by compare_library_snapshot()
# The 'allowed_outcome' is used to indicate when to run without error:
#   allowed_outcome = "snapshot_behind" for snapshot_library()
#   allowed_outcome = "library_behind" for restore_library()
check_snapshot_restore <- function(proj_libpath, allowed_outcome) {
    
    # get comparison info on snapshot and package library
    comparison_outcome <- compare_library_snapshot(proj_libpath)
    
    # stop with error messsage if different from the allowed outcome
    if (!(names(comparison_outcome[1]) %in% allowed_outcome)) {
        # also show the comparison data frame if it exists
        if (length(comparison_outcome) == 2) print(comparison_outcome[[2]])
        stop(comparison_outcome[[1]], call. = FALSE)
    }
    
    # return comparison_outcome
    comparison_outcome
}

#' Create a snapshot (csv) of packages installed in project library
#' 
#' The snapshot improves project portability by storing details about installed
#' packages. The project library can be restored on a different machine using 
#' \code{\link{restore_library}}
#' @param proj_libpath character: The location of the project library 
#' (note: changing this argument is not recommended)
#' @param replace_snapshot logical: If TRUE, any existing snapshot will be removed
#' and a snapshot will be created based on the current project library
#' (if any packages are installed).
#' @family functions for maintaining project package libraries
#' @import dplyr
#' @export
#' @examples
#' saproj::snapshot_library()
snapshot_library <- function(proj_libpath = .libPaths()[1], replace_snapshot = FALSE) {
    
    # determine allowed outcomes
    if (!replace_snapshot) {
        # typically we only want to update if the snapshot is behind the library
        allowed_outcome <- "snapshot_behind"
    } else {
        # we can force a new snapshot if there are conflicts or the library is behind
        allowed_outcome <- c("conflicts", "snapshot_behind", "library_behind")
    }
    
    # check the comparison info on snapshot and package library
    # (throwing error if different from allowed outcome)
    comparison_outcome <- check_snapshot_restore(proj_libpath, allowed_outcome)
    
    # get snapshot
    installed_packages <- comparison_outcome[["compare_df"]] %>%
        filter(in_library) %>% # in case there are conflicts or the library is behind
        select(Package, Version)
    
    # save a snapshot and view outcome
    if (nrow(installed_packages) > 0) {
        write.csv(installed_packages, file = "snapshot-library.csv", row.names = FALSE)
        
        # view file locations
        cat(paste0("\nSnapshot taken from:\n", normalizePath(proj_libpath), "\n"))
        cat(paste0("\nSnapshot saved to:\n", normalizePath("snapshot-library.csv"), "\n"))
        
        # view snapshot
        cat("\nSnapshot Details:\n")
        utils::read.csv("snapshot-library.csv")
        
    } else {
        #  if there are no packages installed, just remove the existing snapshot
        file.remove("snapshot-library.csv")
    }
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

# helper function to compare repository packages to snapshot
# intended for running with restore_library()
# might export it, not sure
# compare_repo_snapshot("https://cran.rstudio.com") # alternative repo
compare_repo_snapshot <- function(repos = getOption("repos")) {
    
    # get details about snapshot packages
    snapshot <- utils::read.csv("snapshot-library.csv", stringsAsFactors = FALSE) %>%
        mutate(in_snapshot = TRUE)
    
    # pull available package list from repo
    # including only binary packages for simplicity
    repo <- list(
        # available.packages(type = "source", repos = repos) %>% 
        #     data.frame(stringsAsFactors = FALSE),
        available.packages(type = "binary") %>% data.frame(stringsAsFactors = FALSE)
    )
    repo <- bind_rows(repo) %>%
        filter(Package %in% snapshot$Package) %>%
        select(Package, Version) %>%
        mutate(in_repo = TRUE) %>%
        distinct()
    
    full_join(snapshot, repo, by = c("Package", "Version")) %>%
        mutate(
            in_repo = ifelse(is.na(in_repo), FALSE, in_repo),
            in_snapshot = ifelse(is.na(in_snapshot), FALSE, in_snapshot)
        ) %>%
        arrange(Package, Version)
}

#' Restore a snapshot to a package library
#' 
#' This installs only those snapshot packages not included in the library.
#' @inheritParams snapshot_library
#' @param override_version logical: If TRUE, uses the version available in getOption("repos")
#' regardless of the version specified in the snapshot.
#' @family functions for maintaining project package libraries
#' @import dplyr
#' @export
#' @examples
#' restore_library()
restore_library <- function(proj_libpath = .libPaths()[1], override_version = FALSE) {
    
    # check the comparison info on snapshot and package library
    # (throwing error if different from allowed outcome)
    comparison_outcome <- check_snapshot_restore(proj_libpath, "library_behind")
        
    ### restore snapshot by installing to selected library
    
    # get details about packages to install
    pkgs <- compare_repo_snapshot()
    pkgs_needed <- comparison_outcome[["compare_df"]] %>% 
        filter(!in_library)
    pkgs_install <- pkgs %>% 
        semi_join(pkgs_needed, by = "Package")
    
    # check for conflicts between repository and snapshot
    if (any(!pkgs_install$in_repo)) {
        # get details about conflicts
        conflicts <- pkgs_install %>% 
            filter(in_repo != in_snapshot)
        
        # for conflicts: don't install unless override_version == TRUE
        if (override_version) {
            # install anyway
            pkgs_install_available <- pkgs_install %>%
                filter(in_repo)
            install.packages(unique(pkgs_install_available$Package))
            compare_library_snapshot()[[2]] # check after install
            
            # modify the snapshot
            pkgs_install_available <- pkgs_install_available %>% 
                select(Package, Version_new = Version)
            utils::read.csv("snapshot-library.csv", stringsAsFactors = FALSE) %>%
                left_join(pkgs_install_available, by = "Package") %>%
                mutate(Version = ifelse(is.na(Version_new), Version, Version_new)) %>%
                select(-Version_new) %>%
                utils::write.csv(file = "snapshot-library.csv", row.names = FALSE)
            
        } else {
            # don't install packages. instead end with warning
            stop(paste0(
                "One or more packages from the snapshot aren't available in the repository:\n",
                getOption("repos")[[1]], "\n\n",
                paste(capture.output(print(conflicts)), collapse = "\n"),
                "\n\nRun 'restore_library(override_version = TRUE)'\n",
                "to install the repo-available version(s) instead."
            ))
        }
    } else {
        # install as usual since there aren't conflicts
        pkgs_install <- pkgs %>% 
            semi_join(pkgs_needed, by = c("Package", "Version"))
        install.packages(unique(pkgs_install$Package))
        compare_library_snapshot()[[2]] # check after install
    }
}

