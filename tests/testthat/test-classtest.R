test_that("spoofing has the correct class", {
  expect_s3_class(iris$spoof, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(iris$meta, c("tbl_df", "tbl", "data.frame"))
})
