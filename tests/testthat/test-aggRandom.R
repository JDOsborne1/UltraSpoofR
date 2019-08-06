test_that("random generation in range", {
  testvect <- sample(1:100, 10)
  expect_lt(ultraAggRandom(testvect), max(testvect))
  expect_gt(ultraAggRandom(testvect), min(testvect))
})
