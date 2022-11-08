# Install and Load the required packages
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
devtools::has_devel() # Check if successfully installed
usethis::use_mit_license("Xu Xinyi")
usethis::use_roxygen_md()
devtools::document()
