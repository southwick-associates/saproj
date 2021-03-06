% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project.R
\name{setup_project}
\alias{setup_project}
\title{Set project parameters for an in-progress analysis}
\usage{
setup_project(
  project_library,
  inherit_library = FALSE,
  r_version = paste(R.version$major, R.version$minor, sep = ".")
)
}
\arguments{
\item{project_library}{character: Name of project library to use for project packages.
This is required in order to encourage reproducibility and portability.}

\item{inherit_library}{logical: If TRUE, will use an existing project library. 
The default is set to FALSE to avoid accidentally using an existing library.}

\item{r_version}{character: R version (e.g., "3.4.3") to use for the project.
Defaults to currently loaded version.}
}
\description{
This sets up the reproducibility/portability by setting (1) R version
and (2) project library. This information will be stored in '.Rprofile'.
}
\examples{
# saproj::setup_project("new-project-name")
}
\seealso{
Other functions for setting up projects: 
\code{\link{new_project}()},
\code{\link{new_section}()},
\code{\link{update_project}()},
\code{\link{view_projects}()}
}
\concept{functions for setting up projects}
