test_that("multi stage calc works", {
  irisUltra$stages[["IncudingSepalArea"]] <- function(input) dplyr::mutate(input, Sepal.Area = Sepal.Length * Sepal.Width)
  expect_true("Sepal.Area" %in% colnames(ultraCallStagedOutput(irisUltra$stages)))
})
