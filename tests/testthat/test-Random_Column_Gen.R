test_that("Random Generation works for Values", {
  expect_length(ultraRandoColumn(irisUltra, "Sepal.Width")$Sepal.Width, 100)
  expect_false(anyNA(ultraRandoColumn(irisUltra, "Sepal.Width")$Sepal.Width))
  expect_lt(as.numeric(max(irisUltra$spoof$Sepal.Width)), max(irisUltra$origin()$Sepal.Width))
})
