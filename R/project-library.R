# functions for maintaining project package libraries

# TODO - Make a simpler method of project library tracking using snapshot/restore

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

# START HERE
# TODO - finish this function (output messages)

# helper function to check project library and compare to snapshot
# import dplyr
compare_library_snapshot <- function(proj_libpath = .libPaths()[1], 
                                     return_data_frame = FALSE) {
    
    # get installed packages
    library_packages <- data.frame(utils::installed.packages(proj_libpath)) %>%
        select(Package, Version) %>%
        mutate(in_library = TRUE)
    if (nrow(library_packages) > 0) has_library = TRUE else has_library = FALSE
    
    # get current snapshot
    if (file.exists("snapshot-library.csv")) has_snapshot = TRUE else has_snapshot = FALSE
    snapshot_packages <- read.csv("snapshot-library.csv") %>%
        mutate(in_snapshot = TRUE)
    
    # produce comparison (difference) data frame
    # based on 4 possible existence comparisons for library & snapshot
    if (!has_library & !has_snapshot) {
        # probably include an end condition here
        print("Neither: no packages installed or recorded")
    } else if (has_library & has_snapshot) {
        diff <- full_join(library_packages, snapshot_packages, by = c("Package", "Version"))
    } else if (has_library & !has_snapshot) {
        diff <- library_packages %>% mutate(in_snapshot = FALSE)
    } else {
        diff <- snapshot_packages %>% mutate(in_library = FALSE)
    }
    
    # output messages
    diff <- filter(diff, in_library != in_snapshot)
    if (nrow(diff) == 0) {
        print("Snapshot is up-to-date with project library")
    } else if (all(diff$in_snapshot)) {
        print("Packages missing from snapshot: run saproj::snapshot_library() to update.")
    } else if (all(diff$in_library)) {
        print("Packages missing from project library: run saproj::restore_library() to update.")
    } else {
        print("Problem, Conflicts!!!!") # see text written down to fill this in
    }
    print(diff)
    
    if (return_data_frame) {
        diff
    }
}

restore_library <- function() {}
