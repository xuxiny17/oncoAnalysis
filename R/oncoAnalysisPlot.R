#' Plot The Mutation Results
#'
#' A function that plots the mutation result details.
#'
#' @param mutvals A list of mutation detail values of class "mutChecker"
#'    addressing the total number of mutations, the number of Base A mutated,
#'    the number of Base T mutated, the number of Base C mutated,
#'    the number of Base G mutated, the position of each mutation, and
#'    the matrix containing mutation details, respective.
#' @param barcolor An optional parameter which determines the bar color.
#' @param title_name An optional parameter which determines the title name of
#'    the plot.
#' @param x_name An optional parameter which determines x axis label.
#' @param y_name An optional parameter which determines y axis label.
#'
#' @return Returns a plot of mutation details.
#'
#' @examples
#' # Example 1:
#' # Using sampleseq and samplemutseq dataset available with package
#' \dontrun{
#' # Read in data
#' load("./data/sampleseq.rda")
#' load("./data/samplemutseq.rda")
#' sampleseq1 <- sampleseq[seq_len(length(sampleseq))]
#' samplemutseq1 <- samplemutseq[seq_len(length(samplemutseq))]
#'
#' # Check the number of mutations.
#' mutCheckerResults <- mutChecker(sampleseq1, samplemutseq1)
#'
#' mutPlot(mutvals = mutCheckerResults)
#' }
#'
#' # Example 2:
#' # Import data in sample raw FASTA format
#' # Note: to read FASTA files, it requires installation of seqinr R package
#' library("seqinr")
#' inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
#' inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")
#'
#' # Read using read.fasta()
#' sampleseq <- seqinr::read.fasta(file = inputhea)
#' samplemutseq <- seqinr::read.fasta(file = inputmut)
#'
#' # Process and store data
#' sampleseq1 <- sampleseq$Sample[seq_len(length(sampleseq$Sample))]
#' samplemutseq1 <- samplemutseq$Samplemut[seq_len(length(samplemutseq$Samplemut))]
#'
#' # Check the number of mutations.
#' mutCheckerResults <- mutChecker(
#'                           sampleseq1,
#'                           samplemutseq1)
#'
#' mutPlot(mutvals = mutCheckerResults)
#'
#' # Example 3:
#' # Import data using fastaReader
#' # Note: to read FASTA files, it requires installation of seqinr R package
#' library("seqinr")
#' inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
#' inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")
#'
#' # Read using fastaReader()
#' sampleseq <- fastaReader(inputhea)
#' samplemutseq <- fastaReader(inputmut)
#'
#' # Check the number of base changes.
#' mutCheckerResults <- mutChecker(sampleseq, samplemutseq)
#'
#' mutPlot(mutvals = mutCheckerResults)
#'
#'@references
#'Holtz, Yan. “Basic Histogram with GGPLOT2.” – The R Graph Gallery.
#'\href{https://r-graph-gallery.com/220-basic-ggplot2-histogram.html}{Link}
#'
#'“Modify Components of a Theme - Theme.” - Theme • ggplot2
#'\href{https://ggplot2.tidyverse.org/reference/theme.html}{Link}
#'
#'“Bar Charts - geom_bar.” - geom_bar • ggplot2
#'\href{https://ggplot2.tidyverse.org/reference/geom_bar.html}{Link}
#'
#'Wickham, Hadley, Winston Chang, and Maintainer Hadley Wickham.
#'"Package ‘ggplot2’." Create elegant data visualisations using the grammar
#'of graphics. Version 2.1 (2016): 1-189.
#'
#'SimonGSimonG4, et al.
#'“‘Correct’ Way to Specifiy Optional Arguments in R Functions.”
#'Stack Overflow, 1 Apr. 1962
#'\href{https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions}{Link}
#'
#'Charif, D. and Lobry, J.R. (2007). SeqinR 1.0-2: a contributed package to the
#'R project for statistical computing devoted to biological sequences retrieval
#'and analysis
#'\href{https://cran.r-project.org/web/packages/seqinr/index.html}{Link}
#'
#'user2588829, et al.
#'“Break/Exit Script.” Stack Overflow, 24 July 2013,
#'\href{https://stackoverflow.com/questions/17837289/break-exit-script}{Link}.
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom seqinr read.fasta
mutPlot <- function(mutvals, barcolor, title_name, x_name, y_name) {
  # Check if the input is valid
  if (typeof(mutvals) != "list" && length(mutvals) != 7) {
    stop("The input of Checker result is invalid.")
  }

  # Setup the default values
  if (missing(barcolor)) {
    barcolor = "#93E9BE"
  }
  if (missing(title_name)) {
    title_name = "Plot of Number of Mutated Bases"
  }
  if (missing(x_name)) {
    x_name = "Base"
  }
  if (missing(y_name)) {
    y_name = "Num of Base mutated"
  }

  data <- NULL

  # Modify the input list
  mutvals <- mutvals[-c(6, 7)]
  # saving data into a dataframe
  PlotingData <- data.frame(data = unlist(mutvals),
                           names = c("Total Num of Mutation",
                                     "A",
                                     "T",
                                     "C",
                                     "G"))

  # plotting the Bar Chart
  plotMut <- ggplot2::ggplot(PlotingData, ggplot2::aes(names, data)) +
    ggplot2::geom_col(fill=barcolor) +
    ggplot2::labs(title = title_name,
                  x = x_name, y = y_name) +
    ggplot2::theme(axis.line =  ggplot2::element_line(colour = "#9FD3BF"),
                   axis.ticks = ggplot2::element_line(linewidth = 1)) +
    ggplot2::geom_text(ggplot2::aes(label = data), size = 3, hjust = 0.5,
                       vjust = 3, color = "#014421")

  return(plotMut)
}

#' Visualize the Base number comparison between Healthy and Mutated Sequence.
#'
#' A function that visualize the base number comparison between healthy
#' and mutated Sequence.
#'
#' @param datahea A character vector containing letters A,C,T,G that
#'    corresponds to control/healthy DNA sequence.
#' @param datamut A character vector containing letters A,C,T,G that
#'    corresponds to suspect mutated DNA sequence.
#' @param title_name An optional parameter which determines the title name of.
#'    the plot
#' @param x_name An optional parameter which determines x axis label.
#' @param y_name An optional parameter which determines y axis label.
#'
#' @return Returns a plot of Sequence comparison.
#'
#' @examples
#' # Example 1:
#' # Using sampleseq and samplemutseq dataset available with package
#' \dontrun{
#' # Read in data
#' load("./data/sampleseq.rda")
#' load("./data/samplemutseq.rda")
#' sampleseq1 <- sampleseq[seq_len(length(sampleseq))]
#' samplemutseq1 <- samplemutseq[seq_len(length(samplemutseq))]
#'
#' # Compare the base numbers in Sequence.
#' mutCompPlot(sampleseq1, samplemutseq1)
#' }
#'
#' # Example 2:
#' # Import data in sample raw FASTA format
#' # Note: to read FASTA files, it requires installation of seqinr R package
#' library("seqinr")
#' inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
#' inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")
#'
#' # Read using read.fasta()
#' sampleseq <- seqinr::read.fasta(file = inputhea)
#' samplemutseq <- seqinr::read.fasta(file = inputmut)
#'
#' # Process and store data
#' sampleseq1 <- sampleseq$Sample[seq_len(length(sampleseq$Sample))]
#' samplemutseq1 <- samplemutseq$Samplemut[seq_len(length(samplemutseq$Samplemut))]
#'
#' # Check the number of mutations.
#' mutCheckerResults <- mutChecker(
#'                           sampleseq1,
#'                           samplemutseq1)
#'
#' # Compare the base numbers in Sequence.
#' mutCompPlot(sampleseq1, samplemutseq1)
#'
#' # Example 3:
#' # Import data using fastaReader
#' # Note: to read FASTA files, it requires installation of seqinr R package
#' library("seqinr")
#' inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
#' inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")
#'
#' # Read using fastaReader()
#' sampleseq <- fastaReader(inputhea)
#' samplemutseq <- fastaReader(inputmut)
#'
#' # Check the number of base changes.
#' mutCheckerResults <- mutChecker(sampleseq, samplemutseq)
#'
#' # Compare the base numbers in Sequence.
#' mutCompPlot(sampleseq, samplemutseq)
#'
#'@references
#'Holtz, Yan. “Basic Histogram with GGPLOT2.” – The R Graph Gallery.
#'\href{https://r-graph-gallery.com/220-basic-ggplot2-histogram.html}{Link}
#'
#'“Modify Components of a Theme - Theme.” - Theme • ggplot2
#'\href{https://ggplot2.tidyverse.org/reference/theme.html}{Link}
#'
#'“Bar Charts - geom_bar.” - geom_bar • ggplot2
#'\href{https://ggplot2.tidyverse.org/reference/geom_bar.html}{Link}
#'
#'Chang, Winston. “R Graphics Cookbook, 2nd Edition.”
#'3.9 Adding Labels to a Bar Graph, 11 Nov. 2022
#'\href{https://r-graphics.org/recipe-bar-graph-labels}{Link}
#'
#'AndrewAndrew 1322 silver badges33 bronze badges, and jtr13jtr13
#'1. “Ggplot geom_col Legend Not Showing.” Stack Overflow, 1 Mar. 1965.
#'\href{https://stackoverflow.com/questions/48134472/ggplot-geom-col-legend-not-showing}{Link}
#'
#'Wickham, Hadley, Winston Chang, and Maintainer Hadley Wickham.
#'"Package ‘ggplot2’." Create elegant data visualisations using the grammar
#'of graphics. Version 2.1 (2016): 1-189.
#'
#'SimonGSimonG4, et al.
#'“‘Correct’ Way to Specifiy Optional Arguments in R Functions.”
#'Stack Overflow, 1 Apr. 1962
#'\href{https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions}{Link}
#'
#'Charif, D. and Lobry, J.R. (2007). SeqinR 1.0-2: a contributed package to the
#'R project for statistical computing devoted to biological sequences retrieval
#'and analysis
#'\href{https://cran.r-project.org/web/packages/seqinr/index.html}{Link}
#'
#'user2588829, et al.
#'“Break/Exit Script.” Stack Overflow, 24 July 2013,
#'\href{https://stackoverflow.com/questions/17837289/break-exit-script}{Link}.
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom seqinr read.fasta
mutCompPlot <- function(datahea, datamut, title_name, x_name, y_name) {
  # Check if input is valid
  if (typeof(datahea) != "character") {
    stop("The Type of Healthy Sequence is invalid, please use character type.")
  } else if (typeof(datamut) != "character") {
    stop("The Type of Mutated Sequence is invalid, please use character type.")
  }

  # Setup the default values
  if (missing(title_name)) {
    title_name = "Plot of the Base Number Comparison"
  }
  if (missing(x_name)) {
    x_name = "Base"
  }
  if (missing(y_name)) {
    y_name = "Frequency"
  }

  if (length(datahea) < length(datamut)) {
    print("Note: Insertion happened.")
  } else if (length(datahea) > length(datamut)) {
    print("Note: Deletion happened.")
  }

  # Modify and clean the data
  healthy1 <- data.frame(table(datahea))
  mut1 <- data.frame(table(datamut))
  colnames(healthy1) <- c("Base", "Frequency")
  colnames(mut1) <- c("Base", "Frequency")
  healthy2 <- data.frame(c("A_Healthy", "C_Healthy", "G_Healthy", "T_Healthy"),
                         healthy1$Frequency)
  mut2 <- data.frame(c("A_Mut", "C_Mut", "G_Mut", "T_Mut"), mut1$Frequency)
  colnames(healthy2) <- c("Base", "Frequency")
  colnames(mut2) <- c("Base", "Frequency")
  combdata <- rbind(healthy2, mut2) # Create the plotting data set
  colnames(mut2) <- c("Base", "Frequency")

  # plotting the Bar Chart
  plotCompMut <- ggplot2::ggplot(combdata, ggplot2::aes(Base, Frequency,
                                                   fill = Base)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = title_name,
                  x = x_name, y = y_name) +
    ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth = 1)) +
    ggplot2::geom_text(ggplot2::aes(label = Frequency), size = 3, hjust = 0.5,
                       vjust = 3, color = "#333333")

  return(plotCompMut)
}

# [END]
