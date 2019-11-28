test_that("Spoofing is the correct size", {
  expect_equal(length(colnames(ultraSpoof(UltraSpoofR::irisUltra)$spoof)), 5)
  expect_equal(nrow(ultraSpoof(UltraSpoofR::irisUltra)$spoof), 100)
})

test_that("Spoofing has correct ranges", {
  test_data <- ultraSpoof(UltraSpoofR::irisUltra)$spoof
  expect_lt(as.numeric(max(test_data$Sepal.Width)), ultraGetMax(UltraSpoofR::irisUltra, "Sepal.Width"))
  expect_gt(as.numeric(min(test_data$Sepal.Width)), ultraGetMin(UltraSpoofR::irisUltra, "Sepal.Width"))
})

test_that("Spoofing has the correct Type", {
  test_data <- ultraSpoof(UltraSpoofR::irisUltra)$spoof
  expect_equal(class(test_data[["Species"]]) , "factor")
  expect_equal(class(test_data[["Sepal.Length"]]) , "numeric")
  expect_equal(class(test_data[["Petal.Length"]]) , "numeric")
  expect_equal(class(test_data[["Sepal.Width"]]) , "numeric")
  expect_equal(class(test_data[["Petal.Width"]]) , "numeric")
})
