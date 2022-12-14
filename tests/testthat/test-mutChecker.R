library("oncoAnalysis")
library("seqinr")

test_that("mutChecker output", {

  inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
  inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")
  inputins <- system.file("extdata", "sampleIns.fasta", package = "oncoAnalysis")

  # Read using read.fasta() or fastaReader
  sampleseq <- seqinr::read.fasta(file = inputhea)
  samplemutseq <- seqinr::read.fasta(file = inputmut)
  sampleinsseq <- oncoAnalysis::fastaReader(inputins)

  # Process and store data
  sampleseq1 <- sampleseq$Sample[seq_len(length(sampleseq$Sample))]
  samplemutseq1 <- samplemutseq$Samplemut[seq_len(length(samplemutseq$Samplemut))]

  # Check the number of mutations.
  mutCheckerResults <- mutChecker(sampleseq1, samplemutseq1)
  mutCheckerResultsins <- mutChecker(sampleseq1, sampleinsseq)


  # Check expected type
  expect_type(mutCheckerResults, "list")
  expect_s3_class(mutCheckerResults, "mutChecker")
  expect_length(mutCheckerResults, 7)
  expect_identical(trunc(mutCheckerResults$TotalMutNum), 9)
  expect_identical(trunc(mutCheckerResults$NumofAmut), 0)
  expect_identical(trunc(mutCheckerResults$NumofTmut), 2)
  expect_identical(trunc(mutCheckerResults$NumofCmut), 4)
  expect_identical(trunc(mutCheckerResults$NumofGmut), 3)
  expect_type(mutCheckerResults$Mutposition, "double")
  expect_type(mutCheckerResults$MutMatrix, "double")
  expect_type(mutCheckerResultsins, "character")
})

context("Checking for invalid user input for mutChecker")
test_that("mutChecker error upon invalid user input", {

  inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
  inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")

  # Read using read.fasta()
  sampleseq <- seqinr::read.fasta(file = inputhea)
  samplemutseq <- seqinr::read.fasta(file = inputmut)


  # Process and store data
  sampleseq1 <- sampleseq$Sample[seq_len(length(sampleseq$Sample))]
  samplemutseq1 <- samplemutseq$Samplemut[seq_len(length(samplemutseq$Samplemut))]

  # Incorrect Healthy Seq input
  expect_error(mutCheckerResults <- mutChecker(1, samplemutseq1))

  # Incorrect Mutated Seq input
  expect_error(mutCheckerResults <- mutChecker(sampleseq1, 1))

  # Incorrect Both Seq input
  expect_error(mutCheckerResults <- mutChecker(sampleseq, samplemutseq))
  expect_error(mutCheckerResults <- mutChecker(1, 2))
})


test_that("mutTable output", {

  inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
  inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")

  # Read using read.fasta()
  sampleseq <- seqinr::read.fasta(file = inputhea)
  samplemutseq <- seqinr::read.fasta(file = inputmut)

  # Process and store data
  sampleseq1 <- sampleseq$Sample[seq_len(length(sampleseq$Sample))]
  samplemutseq1 <- samplemutseq$Samplemut[seq_len(length(samplemutseq$Samplemut))]

  # Check the number of mutations.
  mutCheckerResults <- mutChecker(sampleseq1, samplemutseq1)

  # Check expected type
  expect_type(mutCheckerResults, "list")
  expect_s3_class(mutCheckerResults, "mutChecker")
  expect_length(mutCheckerResults, 7)
  expect_identical(trunc(mutCheckerResults$TotalMutNum), 9)
  expect_identical(trunc(mutCheckerResults$NumofAmut), 0)
  expect_identical(trunc(mutCheckerResults$NumofTmut), 2)
  expect_identical(trunc(mutCheckerResults$NumofCmut), 4)
  expect_identical(trunc(mutCheckerResults$NumofGmut), 3)
  expect_type(mutCheckerResults$Mutposition, "double")
  expect_type(mutCheckerResults$MutMatrix, "double")

  # Build the table
  table_mut <- mutTable(mutCheckerResults$MutMatrix)

  # Check expected type
  expect_type(table_mut, "double")
})

context("Checking for invalid user input for mutTable")
test_that("mutTable error upon invalid user input", {

  inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
  inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")

  # Read using read.fasta()
  sampleseq <- seqinr::read.fasta(file = inputhea)
  samplemutseq <- seqinr::read.fasta(file = inputmut)

  # Process and store data
  sampleseq1 <- sampleseq$Sample[seq_len(length(sampleseq$Sample))]
  samplemutseq1 <- samplemutseq$Samplemut[seq_len(length(samplemutseq$Samplemut))]

  # Check the number of mutations.
  mutCheckerResults <- mutChecker(sampleseq1, samplemutseq1)

  # Incorrect Random Input
  expect_error(table_mut <- mutTable("AAA"))
})


# [END]
