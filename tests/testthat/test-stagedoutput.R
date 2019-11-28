test_that("multi stage calc works", {
  testfile <- UltraSpoofR::irisUltra
  testfile$stages[["IncudingSepalArea"]] <- function(input) dplyr::mutate(input, Sepal.Area = Sepal.Length * Sepal.Width)
  expect_true("Sepal.Area" %in% colnames(ultraCallStagedOutput(UltraSpoofR::irisUltra$stages)))
})
