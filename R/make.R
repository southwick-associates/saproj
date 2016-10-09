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
#' saproj::new_section("1-first-section", projdir = "test-dir")
new_section <- function(title, projdir = NULL) {
    dir_create(title, projdir)
    dir_create(file.path("data", title), projdir)
    dir_create(file.path("doc", title), projdir)
    dir_create(file.path("out", title), projdir)
}

#' Copy selected package (zipfile) to "ref" directory
#'
#' Insures that packages not on CRAN are stored locally if the workflow needs
#' to be rerun. This function automates that process.
#' @param pkg character: Name of package to copy
#' @param pkg_dir character: path to directory holding package zipfiles
#' @param version character: Optional version of package to copy. If NULL,
#' the most recent version will be used
#' @inheritParams file_copy
#' @family functions for making directories and files
#' @export
#' @examples
#' saproj::new_project(projdir = "test-dir")
#' saproj::ref("lichist", projdir = "test-dir")
#' saproj::ref("lichist", version = "0.1.5", projdir = "test-dir")
ref <- function(pkg, pkg_dir = "D:/SA/Project_DK/R", version = NULL,
                projdir = NULL) {
    # get file path (with appropriate version)
    f <- list.files(pkg_dir)
    f <- f[stringr::str_detect(f, ".zip")]
    if (is.null(version)) {
        f <- f[stringr::str_detect(f, pkg)]
        f <- sort(f, decreasing = T)
    } else {
        f <- paste0(pkg, "_", version, ".zip")
    }

    # copy to selected directory
    dir_create("ref", projdir)
    file_copy(file.path(pkg_dir, f[1]), file.path("ref", f[1]), projdir)
}
