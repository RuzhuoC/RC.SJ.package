test_that("search_datasets returns a data frame", {
  result <- search_datasets(c("climate"), max_results = 5)
  expect_s3_class(result, "data.frame")
})

test_that("search_datasets respects max_results", {
  result <- search_datasets(c("climate"), max_results = 10)
  expect_lte(nrow(result), 10)
})
