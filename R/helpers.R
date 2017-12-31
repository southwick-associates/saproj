# helper functions for making files/folders
# these functions aren't exported; they are only used within saproj functions

#' Copy a file
#'
#' This is a helper function for copying files. It is a variation on
#' \code{\link[base]{file.copy}} that includes an extra parameter, projdir
#' @param from character: file path for origin file
#' @param to character: file path for destination file
#' @param projdir character: Subdirectory to place folders/files, intended for
#' testing. If NULL, the working directory will be used
#' @param ... Other parameters passed to \code{\link[base]{file.copy}}
#' @family helper functions for making directories and files
#' @keywords internal
#' @examples
#' file.create("test")
#' saproj::file_copy("test", "test2")
#' saproj::file_copy("test", "test2", projdir = "test-dir")
file_copy <- function(from, to, projdir = NULL, ...) {
    if (!is.null(projdir)) {
        dir.create(projdir, showWarnings = FALSE)
        to <- file.path(projdir, to)
    }
    file.copy(from, to,  ...)
}

#' Make a directory (helper function)
#'
#' A helper function for making directories. A variation on
#' \code{\link[base]{dir.create}} that includes an extra parameter, projdir
#' @param path a character vector containing a single path name
#' @param showWarnings logical; should the warnings on failure be shown?
#' @param ... Other parameters passed to \code{\link[base]{dir.create}}
#' @inheritParams file_copy
#' @family helper functions for making directories and files
#' @keywords internal
#' @examples
#' dir_create("test-dir")
#' dir_create("test-dir", projdir = "proj-dir")
dir_create <- function(path, projdir = NULL, showWarnings = FALSE, ...) {
    if (!is.null(projdir)) {
        dir.create(projdir, showWarnings = showWarnings)
        path <- file.path(projdir, path)
    }
    dir.create(path, showWarnings = showWarnings, ...)
}
