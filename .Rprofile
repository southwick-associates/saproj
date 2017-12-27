# Define project-specific package library & R Version

local({
    
# Project-specific Parameters
r_version <- "3.4.3"
proj_libname <- "southwick-packages"


# Project-agnostic Setup Code ---------------------------------------------

# Prepare Project Library
proj_libpath <- file.path(Sys.getenv("R_HOME"), "project-library", proj_libname)
if (!dir.exists(proj_libpath)) dir.create(proj_libpath, recursive = T)
.libPaths(proj_libpath)

# Print Startup Message
cat("\nProject Packages\n----------------\n")
saproj::view_packages(proj_libpath)

# Print Warning if project R version doesn't match currently loaded R version
r_current_version <- paste(R.version$major, R.version$minor, sep = ".")
if (!(r_version == r_current_version)) {
    msg <- paste(
        "--- Southwick Warning ---\nPlease use R version", r_version,  
        "for this project", "\n  In Rstudio: Tools > Global Options > R Version", 
        "\n\nIf you don't have this version installed, you can download it from", 
        "the 'R Software' Office 365 group:\n", paste0(" Installations/", r_version, ".zip")
    )
    message(paste0("\n", msg))
    
    # Also use a popup warning on windows systems
    if (Sys.info()[["sysname"]] == "Windows") utils::winDialog(type = "ok", message = msg)
}


})     

