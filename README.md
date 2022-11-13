
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oncoAnalysis

An R Package For BCB410H Course work.

<!-- badges: start -->
<!-- https://shields.io/category/license -->
<!-- badges: end -->

## Description

The goal of oncoAnalysis is to …

## Installation

You can install the development version of oncoAnalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("xuxiny17/oncoAnalysis")
```

## Overview

## Contributions

<br> <br> The author of the package is Xu Xinyi. The *mutChecker*
function takes in DNA sequence and generate detailed mutaion
information. The code for *mutChecker* function uses the loop creating
and vector comparing ideas illustrated online (See *References* section
for detiled documentation). The code for *mutTable* function uses the
table creating idea illustrated online to create a two way table.(See
*References* section for detiled documentation). The *mutPlot* and
*mutCompPlot* function make use of the `ggplot2` R package, and the
plotting details imitated the tutorials online along with the technique
of adding optional arguments to the functions (See *References* section
for detiled documentation).

## References

<br> <br> Holtz, Yan. “Basic Histogram with GGPLOT2.” – The R Graph
Gallery. <https://r-graph-gallery.com/220-basic-ggplot2-histogram.html>

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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(oncoAnalysis)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Acknowledgements:

This package was developed as part of an assessment for 2022 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. oncoAnalysis welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the GitHub issues.

<br> <br> The package tree structure is provided below.

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
  |- man
    |- dataGenerater.Rd
    |- mutChecker.Rd
    |- mutCompPlot.Rd
    |- mutPlot.Rd
    |- mutTable.Rd
    |- samplemutseq.Rd
    |- sampleseq.Rd
  |- R
    |- data.R
    |- dataGenerater.R
    |- dataProcessor.R
    |- mutChecker.R
    |- oncoAnalysisCalculation.R
    |- oncoAnalysisPlot.R
  |- vignettes
    |- Introduction_oncoAnalysis.Rmd
  |- tests
    |- testthat.R
    |- testthat
      |- test-mutChecker.R
      |- test-oncoAnalysisPlot.R
```
