test_that("Nominal Split works: MMD example", {
  split_data <- megametadata::metaSplitNominal(megametadata::tester, "AccountLevel")
  expect_equal(split_data$nominal, "Species")
  expect_equal(split_data$continuous, c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ))
})
test_that("Nominal Split works: USR example", {
  split_data <- megametadata::metaSplitNominal(UltraSpoofR::irisUltra$meta)
  expect_equal(split_data$nominal, "Species")
  expect_equal(split_data$continuous, c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ))
})
