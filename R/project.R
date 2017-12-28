# functions for setting up projects

# TODO 
# - Maybe drop the dat, dat2 options
# - parameters: proj_library, inherit_proj_library, R_version (maybe just use current)
# - also include .Rprofile

#' Populate a project with default directories and files
#' @param dat logical: If TRUE, make directory in dat_path
#' @param dat2 logical: If TRUE, make directory in dat_path2
#' @param dat_path character: file path to optional dat directory
#' @param dat_path2 character: file path to optional dat2 directory
#' @inheritParams file_copy
#' @family functions for setting up projects
#' @export
#' @examples
#' saproj::new_project(projdir = "test-dir")
new_project <- function(dat = FALSE, dat2 = FALSE, dat_path = "D:/SA/Data",
                        dat_path2 = "D:/SA/Data2", projdir = NULL) {
    # directories
    dir_create("code", projdir)
    dir_create("data", projdir)
    # dir_create("doc", projdir) # probably won't take this approach generally
    dir_create("out", projdir)
    # dir_create("ref", projdir) # probably only make this when needed
    
    # files
    file_copy(system.file("misc", "README", package = "saproj"),
              "README.txt", projdir)
    
    # data directories (if specified)
    if (dat) make_dat(dat_path)
    if (dat2) make_dat(dat_path2)
}
