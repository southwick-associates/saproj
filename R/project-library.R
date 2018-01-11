# functions for maintaining project package libraries

#' Compare project library to snapshot (helper function)
#' 
#' The result of the comparison informs the recommendation for actions in
#' syncing the package library and it's snapshot. There are 5 possible outcomes
#' to the comparison:
#' \itemize{
#'   \item 'neither' (no packages installed or recorded)
#'   \item 'same' (snapshot up-to-date with library)
#'   \item 'snapshot_behind' (snapshot behind library)
#'   \item 'library_behind' (library behind snapshot)
#'   \item 'conflicts' (library conflicts with snapshot)
#' }
#' @inheritParams snapshot_library
#' @family helper functions for checking project libraries
#' @import dplyr
#' @export
#' @keywords internal
#' @return The function returns a list with 1 or 2 elements:
#' \enumerate{
#' \item [outcome name]: A message about the comparison outcome
#' \item 'compare_df': A data frame comparing snapshot & library packages (if applicable)
#' }
#' @examples
#' saproj::compare_library_snapshot()
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
            "b. reset snapshot with 'saproj::snapshot_library(replace_snapshot = TRUE)'"
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
        snapshot_df <- utils::read.csv("snapshot-library.csv", stringsAsFactors = FALSE,
                                       colClasses = "character") %>%
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

#' Throw an error unless allowed outcome is specified (not exported - internal saproj only)
#' 
#' This is a helper function for use in snapshot_library() and restore_library().
#' It stops function execution for conditions where they shouldn't be run,
#' which is based on the value returned by compare_library_snapshot().
#' For example: one shouldn't (typically) run snapshot_library() unless "snapshot_behind".
#' @param comparison_outcome list: The output of \code{\link{compare_library_snapshot}}
#' @param allowed_outcome character: The allowed outcome name(s) from 
#' \code{\link{compare_library_snapshot}}
#' @family internal helper functions
#' @return Returns the result of compare_library_snapshot()
#' @keywords internal
#' @examples
#' compare_library_snapshot() %>% allow_outcome("snapshot_behind")
#' compare_library_snapshot() %>% allow_outcome("library_behind")
allow_outcome <- function(comparison_outcome, allowed_outcome) {
    
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
    comparison_outcome <- compare_library_snapshot(proj_libpath) %>%
        allow_outcome(allowed_outcome)
    
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



#' List packages installed in library
#' 
#' This is a quick way of looking at packages installed, particularly useful for a 
#' project-specific library.
#' @param library_path character: Path to package library. Defaults to the top-level
#' library (typically a project-specific library if \code{\link{new_project}} was used
#' to initialize a the project).
#' @family functions for maintaining project package libraries
#' @export
#' @examples
#' saproj::view_library()
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


#' Compare snapshot to packages available in repository (helper function)
#' 
#' This is a helper function for use in \code{\link{restore_library}}. 
#' It produces a table comparing snapshot package versions with versions
#' available in the repository. 
#' There are 3 possible outcomes for each package comparison:
#' \itemize{
#'   \item 'same' (snapshot & repository agree)
#'   \item 'missing_from_repo' (snapshot package not available in repository)
#'   \item 'version_conflict' (snapshot & repository have different versions)
#' }
#' @param repos character: The repository searched. The default will likely 
#' come from MRAN (see 'https://mran.microsoft.com/timemachine' for details).
#' @family helper functions for checking project libraries
#' @keywords internal
#' @import dplyr
#' @export
#' @examples
#' saproj::compare_repo_snapshot
compare_repo_snapshot <- function(repos = getOption("repos")) {
    
    # get details about snapshot packages
    if (!file.exists("snapshot-library.csv")) {
        stop("There is no 'snapshot-library.csv' to compare", call. = FALSE)
    }
    snapshot <- utils::read.csv("snapshot-library.csv", stringsAsFactors = FALSE) 
    
    # pull available package list from repo (most recent binary packages only)
    # stops with error if the repo isn't available (determined with warning_flag)
    get_repo_list <- function() available.packages(repos = repos, type = "binary",  
                                                   filters = "duplicates")
    warning_flag <- "unable to access index for repository"
    
    repo_list <- tryCatch(get_repo_list(), warning = function(c) {
        if (stringr::str_detect(conditionMessage(c), warning_flag)) {
            c$message <- paste0(
                "The repo '", repos, "' doesn't appear to be currently available.\n",
                "  You can set a different one with the 'repos' argument. ", 
                "For example:\n", "  repos = 'https://cran.rstudio.com'", "\n\n"
            )
            stop(c)
        }
    }) 
    repo <- data.frame(repo_list, stringsAsFactors = FALSE) %>%
        filter(Package %in% snapshot$Package) %>%
        select(Package, Version_repo = Version)
    
    # define outcomes: available, version_conflict, missing_from_repo
    # these inform how to proceed when restore_library() is run
    snapshot %>%
        rename(Version_snapshot = Version) %>%
        left_join(repo, by = "Package") %>%
        mutate(compare = ifelse(is.na(Version_repo), "missing_from_repo",  
                                ifelse(Version_snapshot != Version_repo,  
                                       "version_conflict", "same"))) %>%
        arrange(Package)
}

#' Restore a project library using a snapshot
#' 
#' This is intended to be run when a project analysis needs to be re-run or edited
#' on a different computer. It installs the packages listed in 'project-snapshot.csv'.
#' @inheritParams snapshot_library
#' @inheritParams compare_repo_snapshot
#' @param override_version logical: If TRUE, uses the version available in repos
#' regardless of the version specified in the snapshot (and updates the snapshot 
#' accordingly).
#' @param use_devtools logical: If TRUE, uses \code{\link[devtools]{install_version}}.
#' This requires the devtools package (obtained via 'install.packages("devtools")').
#' It may also require configuration of your computer 
#' (https://cran.r-project.org/doc/manuals/R-admin.html#The-Windows-toolset)
#' @param devtools_repo character: Repository to use when use_devtools = TRUE
#' @family functions for maintaining project package libraries
#' @import dplyr
#' @export
#' @examples
#' saproj::restore_library()
restore_library <- function(
    proj_libpath = .libPaths()[1], repos = getOption("repos"), override_version = FALSE,  
    use_devtools = FALSE, devtools_repo = "https://cran.rstudio.com"
) {
    
    # check the comparison info on snapshot and package library
    # (throwing error if different from allowed outcome)
    comparison_outcome <- compare_library_snapshot(proj_libpath) %>%
        allow_outcome("library_behind")
    
    # identify packages in snapshot that aren't installed
    pkgs_needed <- comparison_outcome[["compare_df"]] %>% 
        filter(!in_library)
        
    # check the repository for packages to install
    pkgs <- compare_repo_snapshot(repos) %>%
        semi_join(pkgs_needed, by = "Package")
    
    # make error condition for conflicts between snapshot and repository
    if ("version_conflict" %in% pkgs$compare & !override_version & !use_devtools) {
        print_df <- filter(pkgs, compare == "version_conflict")
        stop(paste0(
            "There is a version conflict between the snapshot and the repo:\n  ",
            repos, "\n\n",
            paste(capture.output(print(print_df)), collapse = "\n"), "\n\n",
            "A couple of options for resolving this:\n",
            "a. Use the available repo version with 'override_version = TRUE'\n",
            "b. Install the snapshot version from source using 'use_devtools = TRUE'"
        ))
    }
    
    # send a warning if any snapshot packages are missing from repository
    # still allowing restore_library() to run since the user can install from another repo
    if ("missing_from_repo" %in% pkgs$compare) {
        print_df <- filter(pkgs, compare == "missing_from_repo")
        warning(paste0(
            "A package is missing from the repository and won't be installed:\n\n",
            paste(capture.output(print(print_df)), collapse = "\n")
        ))    
    }
    
    # get final list of packages to install
    pkgs_to_install <- pkgs %>%
        filter(compare != "missing_from_repo")
    if (nrow(pkgs_to_install) == 0) {
        stop("No packages from snapshot to install.\n\n")
    }
    
    # install selected packages    
    if (use_devtools) {
        for (i in seq_along(pkgs_to_install$Package)) {
            devtools::install_version(
                package = pkgs_to_install$Package[i], 
                version = pkgs_to_install$Version_snapshot[i],  
                repos = devtools_repo
            )
        }
    } else {
        install.packages(unique(pkgs_to_install$Package), repos = repos)
    }
    
    # modify snapshot if it was overrided by repo
    if (override_version) {
        utils::read.csv("snapshot-library.csv", stringsAsFactors = FALSE) %>%
            left_join(pkgs_to_install, by = "Package") %>%
            mutate(Version = ifelse(is.na(Version_repo), Version, Version_repo)) %>%
            select(Package, Version) %>%
            utils::write.csv(file = "snapshot-library.csv", row.names = FALSE)
    }
}

