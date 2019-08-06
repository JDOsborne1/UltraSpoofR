test_that("Spoofing is the correct size", {
  expect_equal(length(colnames(ultraSpoof(irisUltra)$spoof)), 5)
  expect_equal(nrow(ultraSpoof(irisUltra)$spoof), 100)
})

test_that("Spoofing has correct ranges", {
  test_data <- ultraSpoof(irisUltra)$spoof
  expect_lt(as.numeric(max(test_data$Sepal.Width)), ultraGetMax(irisUltra, "Sepal.Width"))
  expect_gt(as.numeric(min(test_data$Sepal.Width)), ultraGetMin(irisUltra, "Sepal.Width"))
})

test_that("Spoofing has the correct Type", {
  test_data <- ultraSpoof(irisUltra)$spoof
  expect_type(test_data[["Species"]] , class(irisUltra$origin()[["Species"]]))
  expect_type(test_data[["Sepal.Length"]] , class(irisUltra$origin()[["Sepal.Length"]]))
  expect_type(test_data[["Petal.Length"]] , class(irisUltra$origin()[["Petal.Length"]]))
  expect_type(test_data[["Sepal.Width"]] , class(irisUltra$origin()[["Sepal.Width"]]))
  expect_type(test_data[["Petal.Width"]] , class(irisUltra$origin()[["Petal.Width"]]))
})
