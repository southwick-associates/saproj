
A good reference for making R packages: http://r-pkgs.had.co.nz/

## Functions are separated into 3 files:

- project.R (for setting up projects)
- project-library.R (for maintaining project package libraries)
- saproj.R

## Additional Material

- vignettes/saproj-intro
- inst/rmarkdown (Rmd template)
- inst/misc (template .Rprofile & README)

## Design Decisions

I picked "R.home/project-library" as a place to store project-specific packages because:

- It likely won't be accidently tampered with
- It is machine specific (restablishing on another machine requires installing R and packages)
- It is outside of the project directory:
  - so installed software won't get backed up or transferred to other machines
  - multiple projects can share a project library, useful for big/related projects

I took some time to include lots of messages and error handling. I thought this  was worthwhile since I assume that most users won't follow the snapshot/restore protocol without hand-holding. And I didn't want to add annoying overhead to an analyst's workflow.
