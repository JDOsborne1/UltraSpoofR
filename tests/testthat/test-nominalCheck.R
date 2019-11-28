test_that("Nominal checking works", {
  expect_equal(ultraNominalCheck(irisUltra), c("Species"))
})
test_that("Continuous checking works", {
  expect_equal(ultraContinuousCheck(irisUltra), c("Sepal.Length"
                                                  ,"Sepal.Width"
                                                  ,"Petal.Length"
                                                  ,"Petal.Width" ))
})
