test_that("download_dataset errors with wrong input type", {
  expect_error(download_dataset("not a data frame"), "Input must be a data frame")
})

test_that("download_dataset handles empty input", {
  df_empty <- data.frame(
    title = character(),
    description = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  expect_message(download_dataset(df_empty), "No datasets to download")
})


