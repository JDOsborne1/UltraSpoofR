test_that("spoofing has the correct class", {
  expect_s3_class(irisUltra$spoof, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(irisUltra$meta, c("tbl_df", "tbl", "data.frame"))
})
