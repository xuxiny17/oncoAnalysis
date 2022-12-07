#' Import and Check the FASTA DNA file
#'
#' A function that reads in FASTA files and check if valid.
#'
#' @param filename The file name of the FASTA file wish to be read.
#'
#' @return A Base Characters contained in the sequence. (In real case,
#'    the number of returned items would equal to the length of the input
#'    DNA sequence if sequence is valid.)
#'
#' @examples
#' # Example 1:
#' # Import data in sample raw FASTA format
#' # Note: to read FASTA files, it requires installation of seqinr R package
#' library("seqinr")
#' inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
#' inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")
#'
#' # Read using fastaReader()
#' sampleseq <- fastaReader(inputhea)
#' samplemutseq <- fastaReader(inputmut)
#'
#'@references
#'Charif, D. and Lobry, J.R. (2007). SeqinR 1.0-2: a contributed package to the
#'R project for statistical computing devoted to biological sequences retrieval
#'and analysis
#'
#'mikemike, et al.
#'“Test If Characters Are in a String.” Stack Overflow, 12 Apr. 2012,
#'\href{https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string}{Link}.
#'
#'user2588829, et al.
#'“Break/Exit Script.” Stack Overflow, 24 July 2013,
#'\href{https://stackoverflow.com/questions/17837289/break-exit-script}{Link}.
#'
#' @export
#' @importFrom seqinr read.fasta
fastaReader <- function(filename) {
  # Read the fasta file using read.fasta from package seqinr
  fastafile <- seqinr::read.fasta(file = filename, seqtype = "DNA")
  # Change the name of the variable for further analysis
  names(fastafile) <- "Sequence"

  # Store the sequence
  fastaseq <- fastafile$Sequence[seq_len(length(fastafile$Sequence))]

  # Check if contains invalid DNA base character
  tracker <- 1
  while (tracker <= length(fastaseq)) {
    if (! (grepl(fastaseq[tracker], "ATCGatcg", fixed = TRUE))) {
      stop(sprintf(
        "There exists base: %s which does not belong to ATCG(atcg), please check the FATSA file."
                   , fastaseq[tracker]))
    }
    tracker <- tracker + 1
  }
  return(fastaseq)
}

# [END]
