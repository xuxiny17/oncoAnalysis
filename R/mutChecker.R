#' Check if Mutation Exists along with its numbers
#'
#' A function that checks if there is mutation occurred
#'
#' @param datahea A character vector containing letters A,C,T,G that
#'    corresponds to control/healthy DNA sequence.
#' @param datamut A character vector containing letters A,C,T,G that
#'    corresponds to suspect mutated DNA sequence.
#'
#' @return Returns a list containing mutation details if the length of sequences
#'    are the same.
#' \itemize{
#'   \item The total number of mutations.
#'   \item The number of Base A mutated.
#'   \item The number of Base T mutated.
#'   \item The number of Base C mutated.
#'   \item The number of Base G mutated.
#'   \item The position of each mutation.
#'   \item The matrix containing mutation details. (Role names represent the
#'   base in original sequence, Column names represent the base in
#'   mutated sequence. Used to check for example how many base A has mutated
#'   into base T.)
#' }
#'
#' @examples
#' # Example 1:
#' # Using sampleseq and samplemutseq dataset available with package
#' \dontrun{
#' # Read in data
#' load("./data/sampleseq.rda")
#' load("./data/samplemutseq.rda")
#' sampleseq1 <- sampleseq[1:(length(sampleseq))]
#' samplemutseq1 <- samplemutseq[1:(length(samplemutseq))]
#'
#' # Check the number of mutations.
#' mutCheckerResults <- mutChecker(
#'                           sampleseq1,
#'                           samplemutseq1)
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
#' sampleseq1 <- sampleseq$Sample[1:(length(sampleseq$Sample))]
#' samplemutseq1 <- samplemutseq$Samplemut[1:(length(samplemutseq$Samplemut))]
#'
#' # Check the number of mutations.
#' mutCheckerResults <- mutChecker(
#'                           sampleseq1,
#'                           samplemutseq1)
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
#' # Check the number of mutations.
#' mutCheckerResults <- mutChecker(sampleseq, samplemutseq)
#'
#' @references
#'Charif, D. and Lobry, J.R. (2007). SeqinR 1.0-2: a contributed package to the
#'R project for statistical computing devoted to biological sequences retrieval
#'and analysis
#'\href{https://cran.r-project.org/web/packages/seqinr/index.html}{Link}
#'
#'Coghlan, Avril. "Little book of R for Bioinformatics."
#'Cambridge, UK: Welcome Trust Sanger Institute (2011).
#'\href{https://a-little-book-of-r-for-bioinformatics.readthedocs.io/en/latest/index.html}{Link}.
#'
#'“R Compare Vectors &amp; Find Differences (5 Examples):
#'Identify Similarities.” Statistics Globe, 18 Mar. 2022.
#'\href{https://statisticsglobe.com/compare-vectors-and-find-differences-in-r}{Link}.
#'
#'“R For Loop (with Examples).” DataMentor, 8 Oct. 2018.
#'\href{https://www.datamentor.io/r-programming/for-loop/}{Link}.
#'
#'“How to Create Tables in R?” GeeksforGeeks, 27 Dec. 2021.
#'\href{https://www.geeksforgeeks.org/how-to-create-tables-in-r/}{Link}.
#'
#'user2588829, et al.
#'“Break/Exit Script.” Stack Overflow, 24 July 2013,
#'\href{https://stackoverflow.com/questions/17837289/break-exit-script}{Link}.
#'
#' @export
#' @importFrom seqinr read.fasta
mutChecker <- function(datahea,
                       datamut) {
  # Check if input is valid
  if (typeof(datahea) != "character") {
    stop("The Type of Healthy Sequence is invalid, please use character type.")
  } else if (typeof(datamut) != "character") {
    stop("The Type of Mutated Sequence is invalid, please use character type.")
  }

  # Check if the two vectors are identical.
  if (identical(datahea, datamut)) {
    print("The Two DNA Sequences Are Identical.")
  }

  # Initialize the storage variables.
  tracker <- 1
  count <- 0
  a <- 0
  t <- 0
  c <- 0
  g <- 0
  a_to_t <- 0
  a_to_c <- 0
  a_to_g <- 0
  t_to_a <- 0
  t_to_c <- 0
  t_to_g <- 0
  c_to_a <- 0
  c_to_t <- 0
  c_to_g <- 0
  g_to_a <- 0
  g_to_t <- 0
  g_to_c <- 0
  pos <- c()

  if (length(datahea) > length(datamut)) {
    # Print the type and the number of base deletion
    print("There is Base Deletion")
    differ <- length(datahea) - length(datamut)
    return(print(sprintf("The deletion happened at least: %d times", differ)))
  }

  if (length(datahea) < length(datamut)) {
    # Print the type and the number of base insertion.
    print("There is Base Insertion.")
    return(print(sprintf("The insertion happened at least: %d times",
                         (length(datamut) - length(datahea)))))
  }

  if (length(datahea) == length(datamut)) {
    # Use while loop to track the sequence
    while (tracker <= length(datahea)) {
      # Check if the bases are identical one by one
      if ((! is.na(datahea[tracker])) && (! is.na(datamut[tracker])) &&
           (datahea[tracker] != datamut[tracker])) {
        if (datahea[tracker] == 'a') {
          a <- a + 1 # Calculate the number of mutated base A in original sequence
          if (datamut[tracker] == 't') {
            a_to_t <- a_to_t + 1 # Check mutated to which base
          } else if (datamut[tracker] == 'c') {
            a_to_c <- a_to_c + 1
          } else if (datamut[tracker] == 'g') {
            a_to_g <- a_to_g + 1
          }
        } else if (datahea[tracker] == 't'){
          t <- t + 1 # Calculate the number of mutated base T in original sequence
          if (datamut[tracker] == 'a') {
            t_to_a <- t_to_a + 1
          } else if (datamut[tracker] == 'c') {
            t_to_c <- t_to_c + 1
          } else if (datamut[tracker] == 'g') {
            t_to_g <- t_to_g + 1
          }
        } else if (datahea[tracker] == 'c'){
          c <- c + 1 # Calculate the number of mutated base C in original sequence
          if (datamut[tracker] == 'a') {
            c_to_a <- c_to_a + 1
          } else if (datamut[tracker] == 't') {
            c_to_t <- c_to_t + 1
          } else if (datamut[tracker] == 'g') {
            c_to_g <- c_to_g + 1
          }
        } else if (datahea[tracker] == 'g'){
          g <- g + 1 # Calculate the number of mutated base G in original sequence
          if (datamut[tracker] == 'a') {
            g_to_a <- g_to_a + 1
          } else if (datamut[tracker] == 't') {
            g_to_t <- g_to_t + 1
          } else if (datamut[tracker] == 'c') {
            g_to_c <- g_to_c + 1
          }
        }
        pos <- append(pos, tracker) # Document the position the mutation occurred
        count <- count + 1
      }
      tracker <- tracker + 1
    }

    mutmatrix <- matrix(c(0, a_to_t, a_to_c, a_to_g, t_to_a, 0, t_to_c, t_to_g,
                          c_to_a, c_to_t, 0, c_to_g, g_to_a, g_to_t, g_to_c, 0
    ), ncol=4, byrow=TRUE)

    colnames(mutmatrix) = c('A','T','C','G')
    rownames(mutmatrix) <- c('A','T','C','G')

    # Store the results.
    results <- list(TotalMutNum = count,
                    NumofAmut = a,
                    NumofTmut = t,
                    NumofCmut = c,
                    NumofGmut = g,
                    Mutposition = pos,
                    MutMatrix = mutmatrix)
    class(results) <- "mutChecker"
    return(results)
  }
}


#' Display the Mutation Matrix generated by mutChecker in table format
#'
#' A function that displays Mutation Matrix in table format
#'
#' @param mutatmatrix A mutation matrix generated by mutChecker
#'
#' @return Displays the mutation table.
#' \itemize{
#'   \item A table containing mutation details. (Role names represent the base
#'   in original sequence, Column names represent the base in mutated sequence.
#'   Used to check for example how many base A has mutated into base T.)
#' }
#'
#' @examples
#' # Example 1:
#' # Using sampleseq and samplemutseq dataset available with package
#' \dontrun{
#' # Read in data
#' sampleseq1 <- sampleseq[1:(length(sampleseq))]
#' samplemutseq1 <- samplemutseq[1:(length(samplemutseq))]
#'
#' # Check the number of base changes.
#' mutCheckerResults <- mutChecker(
#'                           sampleseq1,
#'                           samplemutseq1)
#' mutTable(mutCheckerResults$MutMatrix)
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
#' sampleseq1 <- sampleseq$Sample[1:(length(sampleseq$Sample))]
#' samplemutseq1 <- samplemutseq$Samplemut[1:(length(samplemutseq$Samplemut))]
#'
#' # Check the number of base changes.
#' mutCheckerResults <- mutChecker(
#'                           sampleseq1,
#'                           samplemutseq1)
#' mutTable(mutCheckerResults$MutMatrix)
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
#' mutTable(mutCheckerResults$MutMatrix)
#'
#' @references
#'Coghlan, Avril. "Little book of R for Bioinformatics."
#'Cambridge, UK: Welcome Trust Sanger Institute (2011).
#'\href{https://a-little-book-of-r-for-bioinformatics.readthedocs.io/en/latest/index.html}{Link}.
#'
#'“How to Create Tables in R?” GeeksforGeeks, 27 Dec. 2021.
#'\href{https://www.geeksforgeeks.org/how-to-create-tables-in-r/}{Link}.
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
#' @importFrom seqinr read.fasta
mutTable <- function(mutatmatrix){
  # Check if input is valid
  if (typeof(mutatmatrix) != "double") {
    stop("The type of the input Matrix is invalid, please use type double.")
  }

  muttable <- as.table(mutatmatrix)
  return(muttable)
}

# [END]
