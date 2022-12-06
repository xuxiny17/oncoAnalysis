#' Launch Shiny App For Package oncoAnalysis
#'
#' A function that launches the shiny app for oncoAnalysis package.
#' The shiny app would output the DNA Sequence, Base Change Details,
#' Base Change Table, Mutation Plot and Mutation Comparison Plot if the
#' length of the input sequences is the same. (Codes adapted and imitated
#' the function in runMPLNClust.R file for MPLNClust package
#' (Silva, A. et al. (2019)).)
#'
#' @return No return value but open up the shiny page.
#'
#' @examples
#' \dontrun{
#' runoncoAnalysis()
#' }
#'
#' @author Xu Xinyi, \email{tiffanyandjojo.xu@mail.utoronto.ca}
#'
#' @references
#' Silva, A. et al. (2019). A multivariate Poisson-log normal mixture model
#' for clustering transcriptome sequencing data. \emph{BMC Bioinformatics} 20.
#' \href{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-2916-0}{Link}
#'
#' “Function Reference Version 1.0.5.” Shiny,
#' \href{https://shiny.rstudio.com/reference/shiny/1.0.5/}{Link}
#'
#' @export
#' @importFrom shiny runApp
runoncoAnalysis <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "oncoAnalysis")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}

# [END]
