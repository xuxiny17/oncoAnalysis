#' Plot The Mutation Results
#'
#' A function that plots the mutation result details.
#'
#' @param infValues A list of mutation detail values of class "mutChecker"
#'    addressing the total number of mutations, the number of Base A mutated,
#'    the number of Base T mutated, the number of Base C mutated,
#'    the number of Base G mutated, the position of each mutation, and
#'    the matrix containing mutation details, respective.
#'  @param barcolor An optional parameter which determines the bar color.
#'
#' @return Returns a plot of mutation details.
#'
#' @examples
#' # Using sampleseq and samplemutseq dataset available with package
#' # Read in data
#' load("./data/sampleseq.rda")
#' load("./data/samplemutseq.rda")
#' sampleseq1 <- sampleseq[1:(length(sampleseq))]
#' samplemutseq1 <- samplemutseq[1:(length(samplemutseq))]
#'
#' # Check the number of mutations.
#' mutCheckerResults <- mutChecker(sampleseq1,samplemutseq1)
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
#' @export
#' @importFrom ggplot2 ggplot
mutPlot <- function(mutvals, barcolor) {

  if (missing(barcolor)) {
    barcolor = "#93E9BE"
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
  plotMut <- ggplot2::ggplot(PlotingData, aes(names, data)) +
    ggplot2::geom_col(fill=barcolor) +
    ggplot2::labs(title = "Plot of Number of Mutated Bases",
                  x = "Base", y = "Num of Base mutated") +
    ggplot2::theme(axis.line =  ggplot2::element_line(colour = "#9FD3BF"),
                   plot.title = ggplot2::element_text(size = rel(2)),
                   axis.ticks = ggplot2::element_line(linewidth = 1)) +
    ggplot2::geom_text(aes(label = data), size = 3, hjust = 0.5, vjust = 3,
              color = "#014421")

  return(plotMut)
}

#' Visualize the Base number comparison between Healthy and Mutated Sequence.
#'
#' A function that visualize the base number comparison between healthy
#' and mutated Sequence.
#'
#' @param datahea A vector containing DNA sequence(healthy)
#' @param datamut A vector containing DNA sequence(suspect mutated)
#'
#' @return Returns a plot of Sequence comparison.
#'
#' @examples
#' # Using sampleseq and samplemutseq dataset available with package
#' # Read in data
#' load("./data/sampleseq.rda")
#' load("./data/samplemutseq.rda")
#' sampleseq1 <- sampleseq[1:(length(sampleseq))]
#' samplemutseq1 <- samplemutseq[1:(length(samplemutseq))]
#'
#' # Compare the base numbers in Sequence.
#' mutCompPlot(sampleseq1, samplemutseq1)
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
#'#'Wickham, Hadley, Winston Chang, and Maintainer Hadley Wickham.
#'"Package ‘ggplot2’." Create elegant data visualisations using the grammar
#'of graphics. Version 2.1 (2016): 1-189.
#'
#' @export
#' @importFrom ggplot2 ggplot
mutCompPlot <- function(datahea, datamut) {

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


  # plotting the Bar Chart
  plotCompMut <- ggplot2::ggplot(ppp, aes(Base, Frequency, fill = Base)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Plot of the Base Numbers",
                  x = "Base", y = "Frequency") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = rel(2.6)),
                   axis.ticks = ggplot2::element_line(linewidth = 1)) +
    ggplot2::geom_text(aes(label = Frequency), size = 3, hjust = 0.5, vjust = 3,
              color = "#333333")

  return(plotCompMut)
}

# [END]

