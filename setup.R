# This script sets up a renv for the project and installs required packages

# We are using the daily CRAN snapshots from RStudio Package Manager: 
# https://packagemanager.rstudio.com/client/#/repos/1/overview
# Currently, we are using the snapshot from November 29, 2022:
# https://packagemanager.rstudio.com/cran/2022-11-29
# Select the repo snapshot:
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/2022-11-29"))

# Install renv
install.packages("renv")

# Initialize renv for the project
# bare = TRUE: instead of installing dependencies automatically, we install packages manually
renv::init(bare = TRUE)

# Install the packages
Sys.setenv(RENV_DOWNLOAD_METHOD = getOption("download.file.method"))
install.packages(c(
  "dplyr", "readr", "tidyverse", "fitdistrplus", "goft", "mgcv", "ggplot2",
  "tictoc", "itsadug", "moments"
  ))

# Take a snapshot of the renv
renv::snapshot()
