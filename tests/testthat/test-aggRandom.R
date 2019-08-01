test_that("random generation in range", {
  testvect <- sample(1:100, 10)
  expect_less_than(ultraAggRandom(testvect), max(testvect))
  expect_more_than(ultraAggRandom(testvect), min(testvect))
})
