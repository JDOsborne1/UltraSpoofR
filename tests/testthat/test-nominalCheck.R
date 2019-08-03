test_that("Nominal checking works", {
  expect_equal(ultraNominalCheck(irisUltra), c("Species"))
})
