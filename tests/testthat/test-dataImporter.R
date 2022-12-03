test_that("dataImporter output", {

  inputhea <- system.file("extdata", "sample.fasta", package = "oncoAnalysis")
  inputmut <- system.file("extdata", "samplemut.fasta", package = "oncoAnalysis")
  inputdel <- system.file("extdata", "sampleDel.fasta", package = "oncoAnalysis")
  inputins <- system.file("extdata", "sampleIns.fasta", package = "oncoAnalysis")

  # Read using fastaReader()
  sampleseq <- fastaReader(inputhea)
  samplemutseq <- fastaReader(inputmut)
  sampleDelseq <- fastaReader(inputdel)
  sampleInsseq <- fastaReader(inputins)

  # Check the type of the of output.
  expect_type(sampleseq, "character")
  expect_type(samplemutseq, "character")
  expect_type(samplemutseq, "character")
  expect_type(samplemutseq, "character")
})

context("Checking for invalid user input for mutPlot")
test_that("mutPlot error upon invalid user input", {

  inputfalse <- system.file("extdata", "samplefalseseq.fasta",
                            package = "oncoAnalysis")

  # Input incorrect input
  expect_error(fastaReader(inputfalse)) # Invalid character contained
})

# [END]
