test_that("Max getter works", {
  expect_equal(ultraGetMax(irisUltra, "Petal.Width"), 2.5)
})
test_that("Min getter works", {
  expect_equal(ultraGetMin(irisUltra, "Petal.Width"), 0.1)
})
