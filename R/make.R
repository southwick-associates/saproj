# functions to make directories and files

#' Populate a project with default directories and files
#' @param dat logical: If TRUE, make directory in dat_path
#' @param dat2 logical: If TRUE, make directory in dat_path2
#' @param dat_path character: file path to optional dat directory
#' @param dat_path2 character: file path to optional dat2 directory
#' @inheritParams file_copy
#' @family functions for making directories and files
#' @export
#' @examples
#' saproj::new_project(projdir = "test-dir")
new_project <- function(dat = FALSE, dat2 = FALSE, dat_path = "D:/SA/Data",
                        dat_path2 = "D:/SA/Data2", projdir = NULL) {
    # directories
    # dir_create("code", projdir) # probably will store code in parent
    dir_create("data", projdir)
    dir_create("doc", projdir)
    dir_create("out", projdir)
    # dir_create("ref", projdir) # probably only make this when needed

    # files
    file_copy(system.file("misc", "README", package = "saproj"),
              "README", projdir)

    # data directories (if specified)
    if (dat) make_dat(dat_path)
    if (dat2) make_dat(dat_path2)
}

#' Create a new sub-directory for R scripts, docs, etc.
#' @param title character: Name of sub-directory to create
#' @inheritParams file_copy
#' @family functions for making directories and files
#' @export
#' @examples
#' saproj::new_project(projdir = "test-dir")
#' saproj::section("1-first-section", projdir = "test-dir")
section <- function(title, projdir = NULL) {
    dir_create(file.path("code", title), projdir)
    dir_create(file.path("data", title), projdir)
    dir_create(file.path("doc", title), projdir)
    dir_create(file.path("out", title), projdir)
}

#' Copy selected package (zipfile) to a directory
#'
#' Intended to store install files for packages that aren't on CRAN.
#' This function automates storing these files for convenience
#' @param pkg character: Name of package to copy
#' @param pkg_dir character: path to directory holding package zipfiles
#' @param version character: Optional version of package to copy. If NULL,
#' the most recent version will be used
#' @param ref_dir character: Name of directory where packages will be stored
#' @inheritParams file_copy
#' @family functions for making directories and files
#' @export
#' @examples
#' saproj::new_project(projdir = "test-dir")
#' saproj::ref("lichist", projdir = "test-dir")
#' saproj::ref("lichist", version = "0.1.5", projdir = "test-dir")
ref <- function(pkg, pkg_dir = "D:/SA/Project_DK/R", version = NULL,
                ref_dir = "ref", projdir = NULL) {
    # get file path
    f <- list.files(pkg_dir)
    f <- f[stringr::str_detect(f, ".zip")]
    if (is.null(version)) {
        f <- f[stringr::str_detect(f, pkg)]
        f <- sort(f, decreasing = T)
    } else {
        f <- paste0(pkg, "_", version, ".zip")
    }

    # copy to selected directory
    dir_create(ref_dir, projdir)
    file_copy(file.path(pkg_dir, f[1]), file.path(ref_dir, f[1]), projdir)
}
