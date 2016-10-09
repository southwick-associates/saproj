# functions to source R code

#' Source an R script using an Rmd file
#' @param rmd_file character: path to Rmd file used for sourcing
#' @param indir character: path to code sub-directory that contains the R script
#' @param src_file character: name of the R script to source
#' @param title character: Optional title to put at the top of the output file
#' @family functions for sourcing R code
#' @export
#' @examples
#' # (later) maybe include sample R scripts for sourcing
#' rmd()
rmd <- function(rmd_file, indir, src_file, title = "Sourced Code") {
    src_file <- stringr::str_replace(src_file, ".R", "")

    out <- file.path("doc", indir)
    if (!(dir.exists(out))) dir.create(out)

    rmarkdown::render(
        rmd_file,
        output_file = file.path("doc", indir, paste0(src_file, ".html")),
        params = list(file = file.path("code", indir, paste0(src_file, ".R")),
                      set_title = paste(title, src_file, sep = " - "))
    )
}
