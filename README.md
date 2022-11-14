
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oncoAnalysis

Compare DNA Sequences and Check if Mutated.

<!-- badges: start -->
<!-- https://shields.io/category/license -->
<!-- badges: end -->

## Description

The goal of `oncoAnalysis` package is to develop functions that check if
there is mutations between healthy input DNA FASTA file and
mutated(suspected) DNA FASTA file. The package contains the main
components: DESCRIPTION, NAMESPACE, man subdirectory, R
subdirectory(where functions are storing), LICENSE, README, folder
vignettes, tests, data and inst. The package is build for BCB410H
(Applied Bioinformatics) course work. The package is currently under
construction and the current version contains two analysis functions and
two plotting functions. The `oncoAnalysis` package was developed using
`R version 4.2.1 (2022-06-23)`,
`Platform: x86_64-apple-darwin17.0 (64-bit)` and
`Running under: macOS Ventura 13.0.1`.

## Installation

You can install the development version of oncoAnalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("xuxiny17/oncoAnalysis", build_vignettes = TRUE)
library("oncoAnalysis")
```

To run the shinyApp: Under construction

## Overview

ls(“package:oncoAnalysis”) data(package = “oncoAnalysis”)
browseVignettes(“oncoAnalysis”)

`oncoAnalysis` currently contains 4 functions. The *mutChecker* function
(analysis function) takes in two DNA sequence and check if the suspected
mutated sequence is different from the healthy version, if so, output
the mutation details. In details, the output contains the number of
total mutations, the number of Base A, T C, G mutated receptively, the
positions that the mutation occured, and a mutation detailed matrix
containing information which illustrates which base was mutated to which
in mutated sequence compared to the healthy version.

The *mutTable* (analysis function) generates a simple table which
illustrates the matrix output by the *mutChecker* function. The column
names represent the base in original sequence, Role names represent the
base in mutated sequence. Used to check for example how many base A has
mutated into base T. (In future updates, the table is expected to be
made clearer.)

*mutPlot* is a plotting function that plots the number of each base
mutated comparing to the healthy sequence.

The *mutCompPlot* is a plotting function that visualizes the base number
comparison between healthy and mutated Sequence. Both of the plotting
functions could take in optional arguments that specifies the title,
name, x labels and y labels, if not given, the functions would use
default values.

The package also contains two DNA sequencing data sets, called
samplemutseq.rda and sampleseq.rda. Refer to package vignettes for more
details. The current overview of the package is illustrated below.

![](./inst/extdata/Xinyi_X_A1.png)

## Contributions

The author of the package is Xu Xinyi. The *mutChecker* function takes
in DNA sequence and generate detailed mutation information. The code for
*mutChecker* function uses the loop creating and vector comparing ideas
illustrated online (See *References* section for detailed
documentation). The code for *mutTable* function uses the table creating
idea illustrated online to create a two way table.(See *References*
section for detailed documentation). The *mutPlot* and *mutCompPlot*
function make use of the `ggplot2` R package, and the plotting details
imitated the tutorials online along with the technique of adding
optional arguments to the functions (See *References* section for
detailed documentation).

## References

Holtz, Yan. “Basic Histogram with GGPLOT2.” – The R Graph Gallery.
<https://r-graph-gallery.com/220-basic-ggplot2-histogram.html>

Modify Components of a Theme - Theme.” - Theme • ggplot2
<https://ggplot2.tidyverse.org/reference/theme.html>

Bar Charts - geom_bar.” - geom_bar • ggplot2
<https://ggplot2.tidyverse.org/reference/geom_bar.html>

Wickham, Hadley, Winston Chang, and Maintainer Hadley Wickham. Package
‘ggplot2’.” Create elegant data visualisations using the grammar of
graphics. Version 2.1 (2016): 1-189.

Chang, Winston. “R Graphics Cookbook, 2nd Edition.” 3.9 Adding Labels to
a Bar Graph, 11 Nov. 2022
<https://r-graphics.org/recipe-bar-graph-labels>

AndrewAndrew 1322 silver badges33 bronze badges, and jtr13jtr13 1.
“Ggplot geom_col Legend Not Showing.” Stack Overflow, 1 Mar. 1965.
<https://stackoverflow.com/questions/48134472/ggplot-geom-col-legend-not-showing>

Coghlan, Avril. “Little book of R for Bioinformatics.” *Cambridge, UK:
Welcome Trust Sanger Institute (2011)*.
<https://a-little-book-of-r-for-bioinformatics.readthedocs.io/en/latest/index.html>

How to Create Tables in R?” *GeeksforGeeks*, 27 Dec. 2021.
<https://www.geeksforgeeks.org/how-to-create-tables-in-r/>

Charif, D. and Lobry, J.R. (2007). SeqinR 1.0-2: a contributed package
to the R project for statistical computing devoted to biological
sequences retrieval and analysis.
<https://cran.r-project.org/web/packages/seqinr/index.html>

R Compare Vectors & Find Differences (5 Examples): Identify
Similarities.” *Statistics Globe*, 18 Mar. 2022.
<https://statisticsglobe.com/compare-vectors-and-find-differences-in-r>

R For Loop (with Examples).” *DataMentor*, 8 Oct. 2018.
<https://www.datamentor.io/r-programming/for-loop/>

Wickham, H. and Bryan, J. (2019). *R Packages* (2nd edition). Newton,
Massachusetts: O’Reilly Media. <https://r-pkgs.org/>

SimonGSimonG4, et al. “‘Correct’ Way to Specifiy Optional Arguments in R
Functions.” Stack Overflow, 1 Apr. 1962,
<https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions>

## Acknowledgements:

This package was developed as part of an assessment for 2022 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. oncoAnalysis welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the GitHub issues. <br> <br> The
package tree structure is provided below.

``` r
- oncoAnalysis
  |- oncoAnalysis.Rproj
  |- DESCRIPTION
  |- NAMESPACE
  |- LICENSE
  |- README
  |- data
    |- sampleseq.rda
    |- samplemutseq.rda
  |- inst
    CITATION
    |- extdata
      |- Xinyi_X_A1.png
  |- man
    |- mutChecker.Rd
    |- mutCompPlot.Rd
    |- mutPlot.Rd
    |- mutTable.Rd
    |- samplemutseq.Rd
    |- sampleseq.Rd
  |- R
    |- data.R
    |- mutChecker.R
    |- oncoAnalysisPlot.R
  |- vignettes
    |- Introduction_oncoAnalysis.Rmd
  |- tests
    |- testthat.R
    |- testthat
      |- test-mutChecker.R
      |- test-oncoAnalysisPlot.R
```
