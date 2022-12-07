library("oncoAnalysis")
library("seqinr")

test_that("mutPlot output", {

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

  # Plot
  mutationplot <- mutPlot(mutvals = mutCheckerResults)

  # Check expected type
  expect_type(mutationplot, "list")
})

context("Checking for invalid user input for mutPlot")
test_that("mutPlot error upon invalid user input", {

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

  # Input incorrect input
  expect_error(mutationplot <- mutPlot(12345)) # Number
  expect_error(mutationplot <- mutPlot("AAAA")) # String
})

test_that("mutCompPlot output", {

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
  mutcomplot <- mutCompPlot(sampleseq1, samplemutseq1)

  # Check expected type
  expect_type(mutcomplot, "list")
})

context("Checking for invalid user input for mutCompPlot")
test_that("mutCompPlot error upon invalid user input", {

  inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
  inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")

  # Read using read.fasta()
  sampleseq <- seqinr::read.fasta(file = inputhea)
  samplemutseq <- seqinr::read.fasta(file = inputmut)

  # Process and store data
  sampleseq1 <- sampleseq$Sample[seq_len(length(sampleseq$Sample))]
  samplemutseq1 <- samplemutseq$Samplemut[seq_len(length(samplemutseq$Samplemut))]

  # Incorrect Random Input
  expect_error(mutcomplot <- mutCompPlot(1234, samplemutseq1))
  expect_error(mutcomplot <- mutCompPlot(sampleseq1, 1234))
})


# [END]
